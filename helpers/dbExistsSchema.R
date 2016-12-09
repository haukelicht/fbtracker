
dbExistsSchema <- function(conn = con, schema.name) {
  stopifnot(class(con) == "PostgreSQLConnection")
  require(DBI)
  out <- tryCatch(DBI::dbGetQuery(con, 
                                  sprintf("select schema_name from information_schema.schemata where schema_name = '%s';", 
                                          schema.name)),
                  error = function (e) e)
  
  if ("error" %in% class(out)) {
    stop(out$message)
  } else if (class(out) == "data.frame") {
    return(ifelse(nrow(out)==1,TRUE,FALSE))
  } 
  NULL
}