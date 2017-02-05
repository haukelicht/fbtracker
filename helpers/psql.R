
#' @importFrom DBI dbGetQuery

psql <- function(statement = "select version()", conn, simplify = TRUE) { 
  
  # stopifnot(class(conn) == "PostgreSQLConnection")
  # require(DBI)
  
  out <- tryCatch(DBI::dbGetQuery(conn = conn, statement),
                  error = function (err) err)
  
  if (inherits(out, "error")) 
    stop(out$message)
  else if (simplify && !is.null(out) && ncol(out) == 1L) 
    return(as.vector(out[, 1])) 
  else 
  out
} 
