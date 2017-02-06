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

which(page_ids %in% "72364069062")

# REQUEST DATA
start <- Sys.time()

out <- list()

pb <- txtProgressBar(style = 3)
for (page in page_ids) {
  setTxtProgressBar(pb, page)
  out[[page]] <- try(upsertPagePostsData(page, fb_token, con))
}
close(pb)

# save backup
saveRDS(out, sprintf("./data/backup/posts_data_%s.rds", format(Sys.Date(), "%Y%m%d")))

message("\nJob completed in ", round((td <- Sys.time() - start), 2), " ", attr(td, "units"), ".")

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
