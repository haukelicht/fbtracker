
#' @title Connect to Database
#' 
#' @description Connect to database from inside R.
#' 
#' @import DBI dbConnect
#' 
#' @return If connection is succesfull, a DBI connection object, defined by \code{driver.name}.
connectToDB <- function(driver.name, db.name, credentials.list = NULL, ...)
{
  driver <- tryCatch(eval(driver.name), error = function(err) err)
  if (inherits(driver, "error"))  
    stop(driver$message)
  
  do.call(what = DBI::dbConnect, args = append(list(drv = driver, dbname = db.name), as.list(credentials.list, ...)))
}
