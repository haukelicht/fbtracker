
#' @title Query data from a SQL database connection.
#' 
#' @description Query data from a SQL database connection.
#' 
#' @param statement A unit-length character vector, defining an SQL query statment.
#' 
#' @param conn A database connection object, as returned by \code{\link[DBI]{dbConnect}}
#' 
#' @param simplify Logical. If \code{TRUE}, scalar query result sets 
#'   (i.e., only one row, only one column) are returned as 1d vector, not 2d data frame.
#'   
#' @return The query result set.
#'  
#' @importFrom DBI dbGetQuery
psql <- function(statement = "select version()", conn, simplify = TRUE) { 
  
  out <- tryCatch(DBI::dbGetQuery(conn = conn, statement),
                  error = function (err) err)
  
  if (inherits(out, "error")) 
    stop(out$message)
  else if (simplify && !is.null(out) && ncol(out) == 1L) 
    return(as.vector(out[, 1])) 

  out
} 
