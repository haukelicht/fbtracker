rm(list = ls())

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

# REQUEST DATA
start <- Sys.time()

out <- list()
str(head(out), 2)
for (page in page_ids) {
  out[[page]] <- try(upsertPagePostsData(page, fb_token, con))
}

message("\nJob completed in ", round((td <- Sys.time() - start), 2), " ", attr(td, "units"), ".")

# save backup
backup_file <- sprintf("./data/backup/posts_data_%s.rds", format(Sys.Date(), "%Y%m%d"))

if (!file.exists(backup_file)){
  saveRDS(out, backup_file)
} else {
  warning("File %s already exists in data/backup/", basename(backup_file))
}

# visual instpection
str(out[1],2) 
# NOTE that currently, 
length(out) 

writePostsDataListToDB(x = out, conn = con, db.schema = "posts")
