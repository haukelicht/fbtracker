#' @title Get Facebook post data fastly.
#'
#' @description Does the same as getPostsData, that is, 
#'   gets post, likes, comments and reactions data for a single facebook post, 
#'   only faster by using API batch-request.
#'
#' @inherit getPostsData param details return
#' 
#' @usage
#' \preformatted{
#'  getPostData2(post.ids, token,
#'    post.fields = c("from.fields(name,id)", "message", "created_time", "type", "link"),
#'    likes = TRUE, likes.fields = c("id", "name"),
#'    comments = TRUE, comments.fields = c("id", "from.fields(name,id)" , "created_time", "like_count"),
#'    reactions.summary = TRUE, reactions.types = c("LIKE", "LOVE", "WOW", "HAHA", "SAD", "ANGRY", "THANKFUL"))
#' }

getPostsData2 <- function(post.ids,
                          token,
                          post.fields = c("from.fields(name,id)", "message", "story", "created_time", "type", "link"),
                          likes = TRUE,
                          likes.fields = c("id", "name"),
                          comments = TRUE,
                          comments.fields = c("id", "from.fields(name,id)" , "created_time", "like_count"),
                          reactions.summary = TRUE,
                          reactions.types = c("LIKE", "LOVE", "WOW", "HAHA", "SAD", "ANGRY", "THANKFUL")
){
  # helper
  returnEmtpy <- function() {
    out <- list(post = data.frame())
    
    if (likes)
      out[["likes"]] <- data.frame()
    
    if (comments)
      out[["comments"]] <- data.frame()
    
    timestamp <- as.POSIXlt(Sys.time(), tz = "GMT")
    out[["timestamp"]] <- data.frame(load_timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"),
                                     load_tz = "GMT",
                                     stringsAsFactors = FALSE)
    return(out)
  }
  
  start <- Sys.time()
  
  # work-around wrt batch-request limit of 50 IDs
  loops <- 1:(((n_posts <- length(post.ids)) %/% 50) + 1)
  
  url_base <- paste0("https://graph.facebook.com/?ids=%s&fields=",
                     paste(post.fields, collapse = ","),
                     ",shares.summary(true)",
                     ",comments.summary(true)",
                     ifelse(comments, sprintf(".fields(%s).limit(500)", paste(comments.fields, collapse = ",")), ""),
                     ",likes.summary(true)",
                     ifelse(likes, sprintf(".fields(%s).limit(2000)", paste(likes.fields, collapse = ",")),""),
                     ifelse(reactions.summary, ",reactions.fields(type)",""))
  
  start_idx <- 1L
  postData <- list()
  
  for (loop in loops) {
    idx <- start_idx:(start_idx+49L)
    
    idx <- idx[idx <= n_posts]
    
    url <- sprintf(url_base, paste0(post.ids[idx], collapse = ","))
    temp <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(err) err)
    if (inherits(temp, "error"))
      stop(temp$message)
    
    postData[idx] <- temp
    
    start_idx <- start_idx + 49L
  }
  
  if (length(postData) == 0)
    stop(temp$message)
  
  out <- lapply(postData, function(content) {
    
    
    
    if ((l <- length(content)) == 0) 
      return(returnEmtpy())
    
    out <- list()
    out[["post"]] <- makePostDf(x = content, post.fields)
    
    if (reactions.summary) {
      reactions <- makeReactionsSummary(x = content$reactions, reactions.types)
      temp <- as.data.frame(c(out[["post"]], reactions), stringsAsFactors = FALSE)
      out[["post"]] <- temp
    }
    
    if (likes)
      out[["likes"]] <- makeLikesDf(x = content$likes, likes.fields)
    
    if (comments)
      out[["comments"]] <- makeCommentsDf(x = content$comments, comments.fields)
    
    if (likes && out$post$likes_count >= 2000L || comments && out$post$comments_count >= 500L) {
      
      if(likes)
        url_likes <- content$likes$paging$`next`
      
      if(comments)
        url_comments <- content$comments$paging$`next`
      
      if (likes && !is.null(url_likes)) {
        
        content <- tryCatch(callFBGraphAPI(url = url_likes, token = token),
                            error = function(err) err)
        if (inherits(content, "error"))
          stop(sprintf("%s. Could not request FB Graph API.", content$message))
        
        out[["likes"]] <- rbind(out[["likes"]], makeLikesDf(content, likes.fields))
        
        while (length(content$data) > 0 & !is.null(url <- content$paging$`next`)) {
          
          content <- tryCatch(callFBGraphAPI(url = url, token = token),
                              error = function(err) err)
          if (inherits(content, "error"))
            stop(sprintf("%s. Could not request FB Graph API.", content$message))
          
          out[["likes"]] <- rbind(out[["likes"]], makeLikesDf(content, likes.fields))
        }
      }
      
      if (comments && !is.null(url_comments)) {
        content <- tryCatch(callFBGraphAPI(url = url_comments, token = token),
                            error = function(err) err)
        if (inherits(content, "error"))
          stop(sprintf("%s. Could not request FB Graph API.", content$message))
        
        out[["comments"]] <- rbind(out[["comments"]], makeCommentsDf(content, comments.fields))
        
        while (length(content$data) > 0 & !is.null(url <- content$paging$`next`)) {
          
          content <- tryCatch(callFBGraphAPI(url = url, token = token),
                              error = function(err) err)
          if (inherits(content, "error"))
            stop(sprintf("%s. Could not request FB Graph API.", content$message))
          
          out[["comments"]] <- rbind(out[["comments"]], makeCommentsDf(content, comments.fields))
        }
      }
    }
    
    timestamp <- as.POSIXlt(Sys.time(), tz = "GMT")
    out[["timestamp"]] <- data.frame(load_timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"),
                                     load_tz = "GMT",
                                     stringsAsFactors = FALSE)
  
    out  
  })
  
  names(out) <- post.ids
  
  end <- Sys.time()
  
  message(sprintf("Requests run time: %s", format(round(end - start, 3), nsmall = 3)))
  
  return(setNames(out, post.ids))
}


