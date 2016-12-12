
sapply(list.files("./helpers", full.names = TRUE, pattern="*.R"),source,.GlobalEnv)

library(DBI)
library(RPostgreSQL)

drv <- DBI::dbDriver("PostgreSQL")
con <- DBI::dbConnect(drv, dbname="fbtracker")

if ("posts" != psql("select current_schema")) psql("set search_path to posts;")

## Existing Posts and associated last comments 

### set backview (posts )
backview = 50L

### build query
statement <- paste("with last_comments as (select post_id, max(created_time) last_comment",
                   "from posts.post_comments group by post_id)",
                   "select post_id, last_comment from last_comments",
                   "where to_date(last_comment, 'yyyy-MM-dd') >= to_date('%s', 'yyyy-MM-dd')")
 
### query data
LastComments <- psql(sprintf(statement, Sys.Date()-backview))

### convert timestamps
LastComments$last_comment <- convertFBTimestamp(LastComments$last_comment, convert.to.date = FALSE)

temporaryToken = ""


dfList <- apply(LastComments, 1, function(post) { 
  
  cmnt_ids <- getCommentsIDs(post.id = post["post_id"], 
                             token = temporaryToken, 
                             last.comment.ts = post["last_comment"])
  if (length(cmnt_ids) == 0) return()
  out <- getCommentsData(cmnt_ids, token = temporaryToken)
  # out <- data.frame(post["post_id"], out, stringsAsFactors = FALSE)
  if (nrow(out) == 0) return()
  out$post_id <- post["post_id"]
  out[,c(7,1:6)]
})

out <- do.call(rbind, dfList)
rownames(out) <- NULL
nrow(out)
