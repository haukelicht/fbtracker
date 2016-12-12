
#' @description Posts are associated with a post massage, user likes, and user comments in facebook. 
#' This function gets this data for a set of post IDs.
#' @usage getPostsData(post.ids, token)
#' @param post.ids A character vector of positive length with post IDs
#' @param token An OAuth 2.0 personal access token
#' @return A named list containing three date frames, named `Posts`, `Likes` and `Comments`, respectively,
#' which combine data for all posts in vector `post.ids` (adding load date and time)
#' @import Rfacebook::getPost
getPostsData <- function(post.ids, token){

  output <- replicate(3, vector("list", 0L))
  names(output) <- c("Posts", "Likes", "Comments")
  
  stopifnot(is.vector(post.ids)) 

  for (p in seq_along(post.ids)) {
    
    post_id <- post.ids[p]

    input <- tryCatch(getPostData(post_id, token = token), error = function(e) e)
    
    if ("error" %in% class(input)) next
    
    throughput <- lapply(input, function(df) {
      if (!is.null(df) && nrow(df) != 0) {
        df$post_id <- post_id
        if ("message" %in% names(df)) df$message <- stringi::stri_unescape_unicode(df$message)
        date <- as.POSIXlt(Sys.time(), tz = "GMT")
        
        df$load_date <- format(date, "%Y-%m-%d")
        df$load_time <- format(date, "%H:%M:%S")
      } 
      df
    })
    
    this <- c("post", "likes", "comments")
    
    for (df in seq_along(output)) {
      pos <- length(output[[df]])+1
      if (!is.null(throughput[[this[df]]])) { 
        output[[df]][[pos]] <- throughput[[this[df]]]
      }
    }
    
    rm(list = c("input", "throughput"), inherits = FALSE)
  }
  
  output <- lapply(output, function(x) do.call(rbind, x))
  output
}   

