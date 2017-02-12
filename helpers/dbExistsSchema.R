
#' @title Exists Database Schema.
#' 
#' @description Check wheterh schema exists in database
#' 
#' @param conn A database connection object.
#' 
#' @param schema.name A unit-length character vector, giving the name of the schema
#'   for which existence is to be tested.
#' 
#' @importFrom DBI dbGetQuery
#'
#' @note currently works only for connections of class 'PostgreSQLConnection'.
#' 
#' @return A Logical, indicating whether schema exists on database connection.
dbExistsSchema <- function(conn, schema.name) {
  
  stopifnot(class(con) == "PostgreSQLConnection")

  query <- sprintf("select schema_name from information_schema.schemata where schema_name = '%s';", schema.name) 
  
  out <- tryCatch(DBI::dbGetQuery(con, query),
                  error = function (err) err)
  
  if (inherits(out, "error"))
    stop(out$message)
  else if (is.data.frame(out)) 
    return(ifelse(nrow(out)==1, TRUE, FALSE))
  NULL
}
