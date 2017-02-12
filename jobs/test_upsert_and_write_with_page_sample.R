rm(list = ls())

require(DBI)
require(RPostgreSQL)

## load OAuth 2.0 Facebook access token oject 
load("./data/.fb_token")
## source helpers
sapply(list.files("./helpers", full.names = TRUE, pattern="*.R"), source, .GlobalEnv)
## connect to DBMS 
con <- connectToDB(driver.name = RPostgreSQL::PostgreSQL(), db.name = "fbtracker")




page_ids <- readRDS("./data/pages_sample.rds")

out <- list()
for (page in page_ids) {
  out[[page]] <- try(upsertPagePostsData(page, fb_token, con, schema.name = "test"))
}

writePostsDataListToDB(out, conn = con, db.schema = "test")
