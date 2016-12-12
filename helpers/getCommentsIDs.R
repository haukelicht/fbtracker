
#' @description Get comments associated with a posts
#' @param post.id A post ID
#' @param token A valid temporary or OAuth 2.0 access token
#' @param last.comment.ts Timestamp of last recorded comment, formatted as '%Y-%m-%d %H:%M:%S'. 
#' Defaul uses created_time of actual post. 
#' @return A character vector with comment IDs, for which data can be requested.
getCommentsIDs <- function(post.id, token, last.comment.ts = NULL)
{
  if (missing(post.id)) stop("No post ID given.")
  
  url <- sprintf(paste0("https://graph.facebook.com/", 
                        "%s?fields=comments{created_time}", 
                        "&limit=25"), post.id)
  
  content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
  if ("error" %in% class(content)) {
    stop(sprintf("%s. Could not request FB Graph API.", content$message))
  }
  l <- length(content$comments$data)
  
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
  
  if (l == 0) {
    return(vector("character", 0L)) ### change return type ?
  }
  
  if (is.null(last.comment.ts)) {
    last.comment.ts <- tryCatch(convertFBTimestamp(content$created_time, convert.to.date = FALSE), error = function(e) e)
    if ("error" %in% class(last.comment.tsdate)) { 
      stop(sprintf("%s. Could not convert FB timestamps.", last.comment.tsdate$message))
    }
  }
  
  df <- makeDataDF(content$comments)
  
  content <- content$comments
  
  if (l >= 25) {
    dfList <- list(df)
    while (length(content$data) > 0 & 
           !is.null(content$paging$`next`)) {
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
    }
    df <- do.call(rbind, dfList)
  }
  
  timestamps <- tryCatch(convertFBTimestamp(df$created_time, convert.to.date = FALSE), error = function(e) e)
  if ("error" %in% class(timestamps)) { 
    stop(sprintf("%s. Could not convert FB timestamps.", timestamps$message))
  }
  
  df[timestamps > last.comment.ts, "id"]
}
