

psql <- function(statement = "select version()", conn = con) {
  
  stopifnot(class(conn) == "PostgreSQLConnection")
  require(DBI)
  
  out <- tryCatch(dbGetQuery(conn = conn, statement),
                  error = function (e) e)
  
  if ("error" %in% class(out)) {
    stop(out$message)
  } else if (!is.null(out) && ncol(out) == 1L) {
    return(as.vector(out[, 1])) 
  } 
  out
}
