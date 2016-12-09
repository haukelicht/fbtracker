
library(DBI)
library(RPostgreSQL)

drv <- DBI::dbDriver("PostgreSQL")
con <- DBI::dbConnect(drv, dbname="fbtracker")

if (dbExistsSchema(con, "posts") && "posts" != psql("select current_schema")) psql("set search_path to posts;")


