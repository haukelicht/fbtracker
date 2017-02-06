

require(DBI)
require(RPostgreSQL)

## load OAuth 2.0 Facebook access token oject 
load("./data/.fb_token")
## source helpers
sapply(list.files("./helpers", full.names = TRUE, pattern="*.R"), source, .GlobalEnv)
## connect to DBMS 
con <- connectToDB(driver.name = RPostgreSQL::PostgreSQL(), db.name = "fbtracker")

# PREPARE 
### getting page IDs
page_ids <- getSimpleQuery(conn = con,
                           select = "page_id", 
                           from.table = "pages",
                           from.schema = "users")

### run query to check if page is public
# 
# if (any(rmvd <- which(!page_ids %in% unname(unlist(existing_pages))))) {
#   page_ids[rmvd]
#   ## write to users.pages_rmvd
#   page_ids <- page_ids[-rmvd]
# }

# REQUEST DATA
out <- setNames(lapply(page_ids, 
                       upsertPagePostsData, 
                       token = fb_token, 
                       db.connection = con), 
                page_ids)

# save backup
saveRDS(out, "./data/backup/posts_data_20170205.rds")

# visual instpection
str(out[1],2)
# NOTE that currently, 
length(out)

writePostsDataListToDB(x = out[lengths(out) == 4], conn = con, db.schema = "posts")


length(out[lengths(out) > 4])

test <- out[lengths(out) > 4]

reload <- setNames(lapply(names(test), 
                       upsertPagePostsData, 
                       token = fb_token, 
                       db.connection = con), 
                 names(test))

str(reload, 2)
