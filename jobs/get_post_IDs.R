

sapply(list.files("./helpers", full.names = TRUE, pattern="*.R"),source,.GlobalEnv)


library(DBI)
library(RPostgreSQL)

drv <- DBI::dbDriver("PostgreSQL")
con <- DBI::dbConnect(drv, dbname="fbtracker")

if ("posts" != psql("select current_schema")) psql("set search_path to posts;")

## New Posts and associated Likes and Comments 

### query data on most recent posts for all pages 
statement <- paste("select from_name, from_id, id post_id, substring(created_time,1,10)::date created_date",
                   "from posts.posts", 
                   "where (from_id, substring(created_time,1,10)::date)",
                   "in (select from_id, substring(max(created_time),1,10)::date from posts.posts group by from_id)") 

LastPosts <- psql(statement)

page_ids <- LastPosts$from_id
temporaryToken = ""

## Loop over all pages 
for (page_id in page_ids) { 
  
  last_post_from <- unique(LastPosts$created_date)
  from_date <- ifelse(!is.null(last_post_from), 
                      format(as.Date(last_post_from), "%Y/%m/%d"), 
                      format(Sys.Date(), "%Y/%m/%d"))
  
  cat(sprintf("Processing %s's posts since %s ...", LastPosts[LastPosts$from_id == page_id, "from_name"], from_date))
  
  ### get IDs of posts since (incl.) from_date 
  post_IDs <- getPostIDs(page = page_id, token = temporaryToken, since = from_date) 
  
  ### keep only IDs of posts that are not yet recorded in dbms 
  post_IDs <- setdiff(post_IDs, LastPosts[LastPosts$from_id == page_id, "post_id"])
  
  ### query data fro new posts
  postsData <- getPostsData(post.ids = post_IDs, token = temporaryToken)

  read_lines <- vapply(postsData, nrow, integer(1L))
  
  ### check if DBMS connection stil valid 
  still_connected <- tryCatch(dbGetInfo(con), error = function (e) e)
  if ("error" %in% class(still_connected)) {
    con <- DBI::dbConnect(drv, dbname="fbtracker")
    if ("posts" != psql("select current_schema")) psql("set search_path to posts;")
  }
  
  ### write posts, like, and comment data to DBMS
  tables <- c(Posts = "posts", Likes = "post_likes" , Comments = "post_comments")
  
  for (t in seq_along(tables)) {
    written <- tryCatch(DBI::dbSendQuery(con, DBI::sqlAppendTable(con, tables[t], postsData[[names(tables[t])]], row.names = F)),
                        error = function(e) e)
    
    if ("error" %in% class(written)) {
      write.table(postsData[[names(tables[t])]], 
                  sprintf("./data/backup/fb_%s_%s_%s.csv", tables[t], Sys.Date(), page_id),
                  sep = ";", na = "", row.names = FALSE, fileEncoding = "UTF-8")
    }
  }
  if ("PostgreSQLResult" %in% class(written)) {
    cat(paste(read_lines, tolower(names(read_lines)),collapse = ", "), "written to DBMS.\n")
  } else {
    cat(paste(read_lines, tolower(names(read_lines)),collapse = ", "), "written to ./data/backup/.\n")
  }
  
  ### progress with next page
}

disconnect <- DBI::dbDisconnect(con)
disconnect

