
#' @title Set current database schema
#' 
#' @description Function sets current schema, if not already is current schema
#' 
#' @note Tested only on PostgreSQL Driver
#' 
#' @importFrom DBI 

setCurrentSchema <- function(conn, schema) 
{
  if(!(schema_exists <- dbExistsSchema(conn, schema)))
    stop(sprintf("Schema '%s' does not exist on DB connection.", schema))
  
  is_current_schema <- identical(psql(statement = "select current_schema", 
                                      conn = conn), 
                                 tolower(schema))
  
  if (schema_exists && !is_current_schema)
    psql(statement = sprintf("set search_path to %s;", schema), conn = conn)
  
  message(sprintf("Set '%s' as current_schema on DB connection.", schema))
  invisible()
}
