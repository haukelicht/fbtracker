

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
url <- sprintf("https://graph.facebook.com/?ids=%s&fields=id&include_headers=FALSE", paste(page_ids, collapse = ","))

existing_pages <- callFBGraphAPI(url, token = fb_token, retry = 0L)

if (any(rmvd <- which(!page_ids %in% unname(unlist(existing_pages))))) {
  page_ids[rmvd]
  ## write to users.pages_rmvd
  page_ids <- page_ids[-rmvd]
}

# REQUEST DATA 
out <- setNames(lapply(page_ids, 
                       upsertPagePostsData, 
                       token = fb_token, 
                       db.connection = con), 
                page_ids)

writePostsDataListToDB(x = out, conn = con, db.schema = "posts")

