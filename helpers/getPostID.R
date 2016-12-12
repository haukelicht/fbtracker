
#' @description Get ID(s) of post(s) for a single FB page for a given date range.
#' @usage getPostID(page, token, since = NULL, until = NULL) 
#' @param url An URL of API request
#' @param url A temporary access token or an OAuth 2.0 token
getPostID <- function (page, token, since = NULL, until = NULL) 
{
  url <- sprintf("https://graph.facebook.com/%s/posts?fields=id&limit=25", page)
 
  if (!is.null(until)) url <- paste0(url, "&until=", until)
  
  if (!is.null(since)) url <- paste0(url, "&since=", since)

  content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
  if ("error" %in% class(content)) {
    stop(sprintf("%s. Could not request FB Graph API.", content$message))
  }
  l <- length(content$data)

  error <- 0
  while (length(content$error_code) > 0) {
    # cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
    if ("error" %in% class(content)) {
      stop(sprintf("%s. Could not request FB Graph API.", content$message))
    }
    if (error == 3) {
      stop(content$error_msg)
    }
  }
  
  if (length(content$data) == 0) {
    return(vector("character", 0L))
  }
  df <- makeDataDF(content)
  if (!is.null(since)) {
    dates <- tryCatch(convertFBTimestamp(df$created_time, convert.to.date = TRUE), error = function(e) e)
    if ("error" %in% class(dates)) { 
      stop(sprintf("%s. Could not convert FB timestamps.", dates$message))
    }
    mindate <- min(dates)
    sincedate <- as.Date(since)
  }
  if (is.null(since)) {
    sincedate <- as.Date("1970/01/01")
    mindate <- as.Date(Sys.time())
  }
  
  if (l >= 25) {
    dfList <- list(df)
    while (length(content$data) > 0 & 
           !is.null(content$paging$`next`) & 
           sincedate <= mindate) {
      Sys.sleep(0.5)
      url <- content$paging$`next`
      content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
      if ("error" %in% class(content)) {
        stop(sprintf("%s. Could not request FB Graph API.", content$message))
      }
      l <- l + length(content$data)
      
      error <- 0
      while (length(content$error_code) > 0) {
        Sys.sleep(0.5)
        error <- error + 1
        content <- callFBGraphAPI(url = url, token = token)
        if (error == 3) {
          stop(content$error_msg)
        }
      }
      
      newdf <- makeDataDF(content)
      dfList <- c(dfList, list(newdf))
      if (!is.null(since) & nrow(newdf) > 0) {
        dates <- tryCatch(convertFBTimestamp(newdf$created_time, convert.to.date = TRUE), error = function(e) e)
        if ("error" %in% class(dates)) { 
          stop(sprintf("%s. Could not convert FB timestamps.", dates$message))
        }
        mindate <- min(dates)
      }
    }
    df <- do.call(rbind, dfList)
  }

  if (!is.null(since)) {
    dates <- tryCatch(convertFBTimestamp(df$created_time, convert.to.date = TRUE), error = function(e) e)
    if ("error" %in% class(dates)) { 
      stop(sprintf("%s. Could not convert FB timestamps.", dates$message))
    }
    df <- df[dates >= sincedate, ]
  }
  
  return(df$id)
}


