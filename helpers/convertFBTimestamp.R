
#' @title Convert Facebook timestamp to date or time.
#' @description Timestamps associated with Facebook nodes
#'   and edges have a specific format.
#'   This function takes a vector of facebook timestamps,
#'   and converts it to a formatted date or timestamp vector.
#'
#' @param fb.timestamp A character vector, representing a facebook
#'   edge/node timestamp, formatted like '%Y-%m-%dTHH:MM:SO'
#'
#' @param convert.to.date Logical. If \code{TRUE}, Facebook timestamp is
#'   converted to date, otherwise to timestamp.
#'   Default is \code{TRUE}.
#'
#' @param date.format A unit-length character vector, providing the date format
#'   to which the Facebook timestamp is converted.
#'   Default is "%Y-%m-%d".
#'
#' @param timestamp.format A unit-length character vector, providing
#'   the timestamp format to which the Facebook timestamp is converted.
#'   Default is "%Y-%m-%d %H:%M:%S".
#'
#' @return Character vector of dates (timestamps), formatted as \code{date.format} (\code{timestamp.format}).
#'
#' @examples
#'   convertFBTimstamp(fb.timestamp = "2016-08-31T13:30:12+0000")
convertFBTimestamp <- function(fb.timestamp,
                               convert.to.date = TRUE,
                               date.format = "%Y-%m-%d",
                               timestamp.format = "%Y-%m-%d %H:%M:%S",
                               warn = FALSE)
{
  stopifnot(!missing(fb.timestamp))
  stopifnot(is.atomic(fb.timestamp))
  stopifnot(!is.null(date.format))
  stopifnot(!is.null(timestamp.format))

  date_stdrd <- "\\d{4}-\\d{2}-\\d{2}"
  time_stdrd <- "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"

  converter <- function(x,
                        convert.to.date = TRUE,
                        date.format = NULL,
                        timestamp.format = NULL,
                        warn)
  {
    if(!is.character(x)) {
      x <- tryCatch(as.character(x), error = function(err) err)
      if (inherits(x, "error") && warn) {
        warning(sprintf("Cannot convert. %s must be a character vector.", x))
        return(NA_character_)
      }
    }

    if (convert.to.date) {
      fb_date <- substr(x, 1, 10)
      if (!grepl(date_stdrd, fb_date)){
        if (warn)
          warning(sprintf("Cannot convert to date. %s violates 'YYYY-mm-dd' date format.", x))
        return(NA_character_)
      }
      return(format(as.POSIXct(fb_date, format = "%Y-%m-%d", tz = "GMT"), date.format))
    } else {
      fb_timestamp <- sub("T", " ", sub("\\+.*", "", x))
      if (!grepl(time_stdrd, fb_timestamp)) {
        if (warn)
          warning(sprintf("Cannot convert to timestampt. %s violates 'YYYY-mm-dd HH:MM:SS' format.", x))
        return(NA_character_)
      }
      return(format(as.POSIXct(fb_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), timestamp.format))
    }
  }

  unname(vapply(fb.timestamp, converter, character(1L), convert.to.date, date.format, timestamp.format, warn))
}

