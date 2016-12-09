
helpers <- list.files("./helpers", full.names = TRUE, pattern="*.R")
sapply(helpers,source,.GlobalEnv)

from_date = as.Date("2016/11/01")
to_date = as.Date("2016/12/04")

temporaryToken = ""

Barley_post_IDs <- getPostIDs(page = "katarina.barley", token = temporaryToken, since = from_date)

PostDFs <- getPostsData(post.ids = Barley_post_IDs, token = temporaryToken)

lines_read <- vapply(PostDFs, nrow, integer(1L))



library(DBI)
library(RPostgreSQL)

drv <- DBI::dbDriver("PostgreSQL")
con <- DBI::dbConnect(drv, dbname="fbtracker")

if (dbExistsSchema(con, "posts") && "posts" != psql("select current_schema")) psql("set search_path to posts;")

psql("select current_schema")

buildSimpleQuery(select = "post_id, from_id",
                 from.relation = "post_likes", order.by = "post_id",
                 do.query = TRUE, limit = 10)


fbtracker_posts_tables <- c("posts", "post_likes", "post_comments") 

for(t in seq_along(PostDFs)) { 
  tabname <- fbtracker_posts_tables[t]
  from <- PostDFs[[t]]
  # if (!DBI::dbExistsTable(con, tabname)) {
    DBI::dbWriteTable(con, tabname, from, row.names = F)
  # } else if (!identical(names(from), DBI::dbListFields(con, tabname))) {
  #   DBI::dbRemoveTable(con, tabname)
  #   DBI::dbWriteTable(con, tabname, from, row.names = F)
  # }else{
  #   DBI::dbSendQuery(con, sprintf("truncate table %s", tabname))
  #   DBI::dbSendQuery(con, DBI::sqlAppendTable(con, tabname, from, row.names = F))
  # }
  # DBI::dbSendQuery(con, DBI::sqlAppendTable(con, tabname, from, row.names = F))
  # 
} 


disconnected <- DBI::dbDisconnect(con)

