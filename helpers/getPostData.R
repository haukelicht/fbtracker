
#' @description Get post, likes and comment data for a single facebook post
#' @usage getPostData(post.id, token)
#' @param page A facebook-post ID
#' @param token A valid OAuth 2.0 personal access token
#' @return A list of data frames with elements names `post`, `likes `comment`.
#' * `post` has columns 'post_id', 'from_id', 'from_name', 'message', 'created_time', 'type', 'link' and 'shares_count';
#' * `likes` has columns 'user_id' and 'user_name';
#' * `comments` has columns 'comment_id', 'user_id', 'user_name', 'comment', 'created_time', 'like_count'.
getPostData <- function (post.id, token) 
{
  url <- sprintf(paste0("https://graph.facebook.com/%s?fields=from,message,created_time,type,link,name,shares",
                        ",comments.summary(true).fields(id,from.fields(name,id),message,created_time,like_count).limit(500)",
                        ",likes.summary(true).fields(id,name).limit(2000)"),
                 post.id)

  content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
  if ("error" %in% class(content)) {
    stop(sprintf("%s. Could not request FB Graph API.", content$message))
  }
  
  error <- 0
  while (length(content$error_code) > 0) {
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url = url, token = token)
    if (error == 3) {
      stop(content$error_msg)
    }
  }
  
  if (length(content) == 0) {
    stop("Post could not be found")
  }
  
  out <- list()
  out[["post"]] <- makeDataDF(content)
  
  out[["likes"]] <- makeDataDF(content$likes)
  
  n_likes <- ifelse(!is.null(out$likes), dim(out$likes)[1], 0)
  
  out[["comments"]] <- makeDataDF(content$comments)
  
  n_comments <- ifelse(!is.null(out$comments), dim(out$comments)[1], 0)
  
  if (n_likes >= 2000L  || n_comments >= 500L) {

    url_likes <- content$likes$paging$`next`
    
    url_comments <- content$comments$paging$`next`
    
    if (!is.null(url_likes)) {
      
      content <- tryCatch(callFBGraphAPI(url = url_likes, token = token), error = function(e) e)
      if ("error" %in% class(content)) {
        stop(sprintf("%s. Could not request FB Graph API.", content$message))
      }
      
      out[["likes"]] <- rbind(out[["likes"]], makeDataDF(content))
      n_likes <- dim(out$likes)[1]
      
      while (length(content$data) > 0 & !is.null(url <- content$paging$`next`)) {
        
        content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
        if ("error" %in% class(content)) {
          stop(sprintf("%s. Could not request FB Graph API.", content$message))
        }
        
        out[["likes"]] <- rbind(out[["likes"]], makeDataDF(content))
        n_likes <- dim(out$likes)[1]
      }
    }
    if (!is.null(url_comments)) {
      content <- tryCatch(callFBGraphAPI(url = url_comments, token = token), error = function(e) e)
      if ("error" %in% class(content)) {
        stop(sprintf("%s. Could not request FB Graph API.", content$message))
      }
      
      out[["comments"]] <- rbind(out[["comments"]], makeDataDF(content)) 
      n_comments <- dim(out$comments)[1]
      
      while (length(content$data) > 0 & !is.null(url <- content$paging$`next`)) {
        
        content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
        if ("error" %in% class(content)) {
          stop(sprintf("%s. Could not request FB Graph API.", content$message))
        }
        
        out[["comments"]] <- rbind(out[["comments"]], makeDataDF(content))
        n_comments <- dim(out$comments)[1]
      }
    }
  }
  
  out <- lapply(out, function(df) {
    if ("message" %in% names(df)) {
      df[["message"]] <- iconv(iconv(x = df[["message"]], from = "UTF-8", to = "ISO-8859-1", sub = ""), to = "UTF-8", from = "ISO-8859-1")
    }
    df
  })

  return(out)
}

