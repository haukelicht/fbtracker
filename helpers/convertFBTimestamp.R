
#' @description Timestamps associated with facebook nodes and edges have a specific format. 
#' This function takes a vector of facebook timestamps, and converts it to a formatted date or timestamp string.
#' @usage convertFBTimstamp(fb.timestamp, convert.to.date = TRUE, date.format = "%Y/%m/%d", timestamp.format = "%Y/%m/%d %H:%M:%S")
#' @param fb.timestamp  A facebook edge/node timestamp, formatted like '%Y-%m-%dTHH:MM:SO'
#' @param convert.to.date Convert to date-formatted string. Default is \code{TRUE}; if \code{FALSE}, converts to timestamp-formatted string. 
#' @param date.format A string providing the date format to which the facebook timestamp is converted. Default is '%Y/%m/%d'
#' @param timestamp.format A string providing the timestamp format to which the facebook timestamp is converted. Default is '%Y/%m/%d %H:%M:%S'
#' @return Character vector formatted like specified in \code{date.format} or \code{timestamp.format}.
#' @example convertFBTimstamp(fb.timestamp = '2016-08-31T13:30:12+0000')
convertFBTimestamp <- function(fb.timestamp, 
                              convert.to.date = TRUE,
                              date.format = "%Y/%m/%d",
                              timestamp.format = "%Y/%m/%d %H:%M:%S"){
  
  stopifnot(!missing(fb.timestamp))
  stopifnot(all(vapply(fb.timestamp, is.character, logical(1L))))
  
  date_stdrd <- "\\d{4}-\\d{2}-\\d{2}"
  time_stdrd <- "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"
  
  if (convert.to.date) {
    fb_date <- vapply(fb.timestamp, function(d) substr(d,1,10), character(1L)) 
    if (!all(vapply(fb_date, function(d) grepl(date_stdrd, d), logical(1L)))) {
      stop("Method not applicable if first ten characters of `fb.timestamp` violate 'YYYY-mm-dd' date format.")
    }
    date <- format(as.POSIXct(fb_date, format = "%Y-%m-%d", tz = "GMT"), date.format)
    return(date)
  } else if (!convert.to.date) {
    fb_timestamp <- vapply(fb.timestamp, function(d) sub("\\+.*", "", sub("T", " ", d)), character(1L)) 
    if (!all(vapply(fb_timestamp, function(d) grepl(time_stdrd, d), logical(1L)))) {
      stop("Method not applicable if `fb.timestamp` violates 'YYYY-mm-dd HH:MM:SS' time format.")
    } 
    timestamp <- format(as.POSIXct(fb_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), timestamp.format)
    return(timestamp)
  }

  fb.timestamp
}

