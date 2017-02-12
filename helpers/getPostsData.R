#' @title Get Facebook posts data.
#'
#' @description Get post, likes, comments and reactions data for a set of facebook post.
#'
#' @inheritParams getPostData 
#'
#' @return A list of named lists, each list being a list of data frame object with elements:
#'   \itemize{
#'     \item 'post' has columns as defined by \code{post.fields},
#'       where subfield requests (e.g. 'from.field(name,id)') are deparsed,
#'       such that each subfield is written to its own column, and subfield columns
#'       are named like 'main_subfield' (e.g. 'from_id'; see \code{\link{makePostDf}})
#'       Also, if \code{reactions.summary = TRUE}, columns for each
#'       \code{reactions.types} requested are added, containing post-level aggregates, 
#'       i.e., summary statistics representing count of a given reactions type as off request time.
#'     \item 'likes' has columns as defined by \code{likes.fields}
#'       (see \code{\link{makeLikesDf}})
#'     \item 'comments' has columns as defined by \code{comments.fields}
#'       (see \code{\link{makeCommentsDf}})
#'    \item 'timestamp' has columns 'timestamp' and 'tz', giving the
#'      timestamp ("YYYY-mm-dd HH:MM:SS") and timezone (GMT) when
#'      the posts data was requested.
#'   }
#'   In addition, the output list has 
#' @usage
#' \preformatted{
#'  getPostsData(post.ids, token,
#'    post.fields = c("from.fields(name,id)", "message", "created_time", "type", "link"),
#'    likes = TRUE, likes.fields = c("id", "name"),
#'    comments = TRUE, comments.fields = c("id", "from.fields(name,id)" , "created_time", "like_count"),
#'    reactions.summary = TRUE, reactions.types = c("LIKE", "LOVE", "WOW", "HAHA", "SAD", "ANGRY", "THANKFUL"))
#' }

getPostsData <- function(post.ids,
                         token,
                         post.fields = c("from.fields(name,id)", "message", "story", "created_time", "type", "link"),
                         likes = TRUE,
                         likes.fields = c("id", "name"),
                         comments = TRUE,
                         comments.fields = c("id", "from.fields(name,id)" , "created_time", "like_count"),
                         reactions.summary = TRUE,
                         reactions.types = c("LIKE", "LOVE", "WOW", "HAHA", "SAD", "ANGRY", "THANKFUL")
){
  start <- Sys.time()
  
  out <- lapply(post.ids, getPostData, 
                token = token,
                post.fields = post.fields,
                likes = likes,
                likes.fields = likes.fields,
                comments = comments,
                comments.fields = comments.fields,
                reactions.summary = reactions.summary,
                reactions.types = reactions.types) 
  
  end <- Sys.time()
  
  names(out) <- lapply(out, attr, "post_id")
  message(sprintf("Requests run time: %s", format(round(end - start, 3), nsmall = 3)))
  
  return(out)
}

