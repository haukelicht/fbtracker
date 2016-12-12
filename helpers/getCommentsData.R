
#' @description Get data for list of comment IDs
#' @param comment.ids A vector or list of comment IDs (as returned by getCommentsIDs)
#' @param token A valid temporary or OAuth 2.0 access token
#' @return A data frame with columns 'comment_id', 'user_id', 'user_name', 'comment', 'created_time', 'like_count'.
getCommentsData <- function(cmnt.ids, token)
{
  out <- lapply(cmnt.ids, function(cmnt_id) {
    url <- sprintf(paste0("https://graph.facebook.com/", 
                          "%s?fields=id,from.fields(name,id),message,created_time,like_count"),
                   cmnt_id)
    
    content <- tryCatch(callFBGraphAPI(url = url, token = token), error = function(e) e)
    if ("error" %in% class(content)) {
      out <- c(comment_id = cmnt_id,
               user_id = NA_character_,
               user_name = NA_character_,
               comment = NA_character_, 
               created_time = NA_character_,
               like_count = NA_character_)
    }
    out <- c(comment_id = content[["id"]],
             user_id = content[["from"]][["id"]],
             user_name = content[["from"]][["name"]],
             comment = content[["message"]], 
             created_time = content[["created_time"]],
             like_count = ifelse(is.null(likes <- content[["like_count"]]), "0", likes))
    
    out
  })
  
  return(as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE))
}

       