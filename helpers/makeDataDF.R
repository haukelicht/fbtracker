
#' @description This helper function takes connent of post data query from facebook's graph API,
#' as returned by helper callFBGraphAPI, and returns a named data frame of post, likes or comments data conditional on input
#' @usage makeDataDF(list)
#' @param list Either the sub-list \code{likes}, \code{comments}, or the list itself, returned by helper callFBGraphAPI()
makeDataDF <- function(list) 
{
  if ("data" %in% names(list)) {
    df <- as.data.frame(do.call(rbind, lapply(list[["data"]], unlist)), stringsAsFactors = F)
    if ("from.id" %in% names(df)) { 
      names(df)[1:4] <- c("comment_id", "user_id", "user_name", "comment")
    } else if (length(df) == 2 && c("name", "id") %in% names(df)) {
      names(df)[1:2] <- c("user_id", "user_name")
    }
  } else {
    df <- as.data.frame(c(list["id"],
                          list[["from"]]["id"],
                          list[["from"]]["name"],
                          list[c("message", "created_time", "type", "link")],
                          shares = ifelse(is.null(shares <- list[["shares"]]), list(0), shares["count"])), 
                        stringsAsFactors = F)
    names(df)[c(1:3,8)] <- c("post_id", "from_id", "from_name", "shares_count")
  }
  return(df)
}


