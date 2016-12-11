
getPostData <- function (post.id, token) 
{
  
  url <- sprintf(paste0("https://graph.facebook.com/%s?fields=from,message,created_time,type,link,name,shares",
                        ",comments.summary(true).fields(id,from,message,created_time,like_count).limit(500)",
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
  out[["post"]] <- as.data.frame(c(content$from["id"],
                    content[c("message", "created_time", "type", "link")],
                    shares = ifelse(is.null(shares <- content$shares), list(0), shares["count"]),
                    content["id"]), 
                  stringsAsFactors = F)
  names(out[["post"]])[c(1,6,7)] <- c("from_id", "shares_count", "post_id")
  
  out[["likes"]] <- as.data.frame(do.call(rbind, lapply(content$likes$data, unlist)), 
                                  stringsAsFactors = F)
  n_likes <- ifelse(!is.null(out$likes), dim(out$likes)[1], 0)
  
  out[["comments"]] <- as.data.frame(do.call(rbind, lapply(content$comments$data, unlist)), 
                                     stringsAsFactors = F)
  n_comments <- ifelse(!is.null(out$comments), dim(out$comments)[1], 0)
  
  if (2000L > n_likes || 500L > n_comments) {

    url_likes <- content$likes$paging$`next`
    
    url_comments <- content$comments$paging$`next`
    
    if (!is.null(url_likes)) {
      content <- tryCatch(callFBGraphAPI(url = url_likes, token = token), error = function(e) e)
      if ("error" %in% class(content)) {
        stop(sprintf("%s. Could not request FB Graph API.", content$message))
      }
      out[["likes"]] <- rbind(out[["likes"]], 
                              as.data.frame(do.call(rbind, lapply(content$data, unlist)), 
                                            stringsAsFactors = F))
      n_likes <- dim(out$likes)[1]
      while (length(content$data) > 0 & !is.null(url <- content$paging$`next`)) {
        content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
        if ("error" %in% class(content)) {
          stop(sprintf("%s. Could not request FB Graph API.", content$message))
        }
        out[["likes"]] <- rbind(out[["likes"]], 
                                as.data.frame(do.call(rbind, lapply(content$data, unlist)), 
                                              stringsAsFactors = F))
        n_likes <- dim(out$likes)[1]
      }
    }
    if (!is.null(url_comments)) {
      content <- tryCatch(callFBGraphAPI(url = url_comments, token = token), error = function(e) e)
      if ("error" %in% class(content)) {
        stop(sprintf("%s. Could not request FB Graph API.", content$message))
      }
      out[["comments"]] <- rbind(out[["comments"]], 
                                 as.data.frame(do.call(rbind, lapply(content$data, unlist)), 
                                               stringsAsFactors = F))
      n_comments <- dim(out$comments)[1]
      while (length(content$data) > 0 & !is.null(url <- content$paging$`next`)) {
        content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
        if ("error" %in% class(content)) {
          stop(sprintf("%s. Could not request FB Graph API.", content$message))
        }
        out[["comments"]] <- rbind(out[["comments"]], 
                                   as.data.frame(do.call(rbind, lapply(content$data, unlist)), 
                                                 stringsAsFactors = F))
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

