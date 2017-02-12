#' @title Get timestamp.
#' 
#' @description Returns preformatted timestamp with timezone 'GMT'.
ts <- function() format(as.POSIXlt(Sys.time(), tz = "GMT"), "%Y-%m-%d %H:%M:%S")
