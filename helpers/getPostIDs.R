
#' @title Get post IDs of page.
#'
#' @description Get ID(s) of post(s) for a single Facebook page and a given date range.
#'
#' @param url An URL representing a Facebook Graph API request.
#'
#' @param token A temporary access token or an OAuth 2.0 token.
#'
#' @param since A date, specifying since when post IDs are requested ofr \code{page}.
#'   If \code{NULL}, all post IDs created since page exists are requested.
#'   Otherwise, date format must be "%Y-%m-%d".
#'   Defaults to \code{Sys.Date()-60} (60 days from today).
#'
#' @param until A date, specifying until when post IDs are requested ofr \code{page}.
#'   If \code{NULL}, all post IDs created until this moment are requested.
#'   Otherwise, date format must be "%Y-%m-%d".
#'   Defaults to \code{NULL}.
#'
#' @param newest.first Logical. If \code{TRUE}, newest posts is first row of output.
#'
#' @return A data frame with columns "post_id" and "created_time".
getPostIDs <- function (page, token, since = Sys.Date()-60L, until = NULL, newest.first = TRUE)
{
  isDateInput <- function(x)
  {
    if (inherits(x, "Date")) {
      TRUE
    } else if (inherits(x, "POSIXct")){
      TRUE
    } else if (is.character(x)) {
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x))
        return(!is.na(as.Date(x, format = "%Y-%m-%d")))
      else
        return(FALSE)
    } else {
      return(FALSE)
    }
  }

  stopifnot(is.character(page))

  if (!is.null(since) && (length(since) != 1 || !isDateInput(since)))
    stop("Input passed to 'since' must be unit-length vector of class 'Date' or 'POSIXct', and format '%Y-%m-%d'")

  if (!is.null(until) && (length(until) != 1 || !isDateInput(until)))
    stop("Input passed to 'until' must be unit-length vector of class 'Date' or 'POSIXct', and format '%Y-%m-%d'")

  url <- sprintf("https://graph.facebook.com/%s/posts?fields=id,created_time&limit=50", page)

  if (!is.null(until))
    url <- paste0(url, "&until=", until)

  if (!is.null(since))
    url <- paste0(url, "&since=", since)

  content <- tryCatch(callFBGraphAPI(url = url, token = token),
                      error = function(err) err)
  if (inherits(content, "error"))
    stop(sprintf("%s. Could not request FB Graph API.", content$message))

  if ((l <- length(content$data)) == 0)
    return(data.frame(list()))

  df <- as.data.frame(do.call(rbind, lapply(content$data, unlist)), stringsAsFactors = F)

  if (!is.null(since)) {
    dates <- tryCatch(convertFBTimestamp(df$created_time, convert.to.date = TRUE),
                      error = function(err) err)
    if (inherits(dates, "error"))
      stop(sprintf("%s. Could not convert FB timestamps.", dates$message))

    mindate <- min(dates)
    sincedate <- as.Date(since)
  } else {
    sincedate <- as.Date("1970/01/01")
    mindate <- as.Date(Sys.time())
  }

  if (l >= 50) {
    dfList <- list(df)
    while (length(content$data) > 0 &&
           !is.null(content$paging$`next`) &&
           sincedate <= mindate) {

      Sys.sleep(0.5)

      url <- content$paging$`next`
      content <- tryCatch(callFBGraphAPI(url = url, token = token),
                          error = function(err) err)
      if (inherits(content, "error"))
        stop(sprintf("%s. Could not request FB Graph API.", content$message))

      if(length(content$data) == 0)
        next

      newdf <- as.data.frame(do.call(rbind, lapply(content$data, unlist)), stringsAsFactors = F)
      dfList[[length(dfList)+1L]] <- newdf

      if (!is.null(since) && nrow(newdf) > 0) {
        dates <- tryCatch(convertFBTimestamp(newdf$created_time, convert.to.date = TRUE),
                          error = function(err) err)
        if (inherits(dates, "error"))
          stop(sprintf("%s. Could not convert FB timestamps.", dates$message))

        mindate <- min(dates)
      }
    }

    df <- do.call(rbind, dfList)
  }

  if (!is.null(since)) {
    dates <- tryCatch(convertFBTimestamp(df$created_time, convert.to.date = TRUE),
                      error = function(err) err)
    if (inherits(dates, "error"))
      stop(sprintf("%s. Could not convert FB timestamps.", dates$message))

    df <- df[dates >= sincedate, ]
  }

  df$created_time <- convertFBTimestamp(fb.timestamp = df$created_time, convert.to.date = FALSE)
  names(df) <- c("post_id", "created_time")

  return(df[order(df$created_time, decreasing = newest.first), ])
}


