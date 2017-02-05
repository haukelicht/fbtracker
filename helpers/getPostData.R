
#' @title Get Facebook post data.
#'
#' @description Get post, likes, comments and reactions data for a single facebook post.
#'
#' @param post.id An unit-length character vector, representing a post ID.
#'
#' @param token A valid OAuth 2.0 personal access token.
#'
#' @param post.fields A character vector, specifying the post fields to request.
#'   Defaults to \code{c("from.fields(name,id)", "message", "created_time", "type", "link")}
#'   (see \url{https://developers.facebook.com/docs/graph-api/reference/v2.8/post/} for a list of available fields).
#'   Subfields (e.g., "from" name and ID) must be passed as \code{"main.fields(subfield1,subfield2)"}.
#'
#' @param likes Logical. If \code{TRUE} (the default), post likes are requested.
#'
#' @param likes.fields A character vector, specifying the fields of the likes edge
#'   (see \url{https://developers.facebook.com/docs/graph-api/reference/v2.8/object/likes}).
#'   Defaults to \code{c("name", "id")}, i.e., the name and ID of the user(s), who liked the post.
#'
#' @param comments Logical. If \code{TRUE} (the default), post comments are requested.
#'
#' @param comments.fields A character vector, specifying the fields of the comments edge
#'   (see \url{https://developers.facebook.com/docs/graph-api/reference/v2.8/object/comments}).
##'   Defauts to \code{c("id", "from.fields(name,id)" , "created_time", "like_count")}.
#'
#' @param reactions.summary Logical. If \code{TRUE} (the default), post reactions summary statistics are requested.
#'
#' @param reactions.types A character vector, specifying the types of the reactions edge,
#'   (see \url{https://developers.facebook.com/docs/graph-api/reference/post/reactions}).
#'
#' @return A named list of data frames.
#'   \itemize{
#'     \item "post" has columns as defined by \code{post.fields},
#'       where subfield requests (e.g. "from.field(name,id)") are deparsed,
#'       such that each subfield is written to its own column an subfield columns
#'       are named like "main_subfield" (e.g. "from_id"; see \code{\link{makePostDf}})
#'       Also, if \code{reactions.summary = TRUE}, columns for each
#'       \code{reactions.types} requested are added, containing post-level aggregates, i.e.,
#'       numeric summary statistics representing count of a given reactions type.
#'     \item "likes" has columns as defined by \code{likes.fields}
#'       (see \code{\link{makeLikesDf}})
#'     \item "comments" has columns as defined by \code{comments.fields}
#'       (see \code{\link{makeCommentsDf}})
#'    \item "timestamp" has columns "date", "time", and "tz", giving the
#'      date ("YYYY-mm-dd"), time ("HH:MM:SS") respectively timezone (GZT) when
#'      the post data was requested.
#'   }
#' @usage
#' \preformatted{
#'  getPostData(post.id, token,
#'    post.fields = c("from.fields(name,id)", "message", "created_time", "type", "link"),
#'    likes = TRUE, likes.fields = c("id", "name"),
#'    comments = TRUE, comments.fields = c("id", "from.fields(name,id)" , "created_time", "like_count"),
#'    reactions.summary = TRUE, reactions.types = c("LIKE", "LOVE", "WOW", "HAHA", "SAD", "ANGRY", "THANKFUL"))
#' }

getPostData <- function (post.id, token,
                         post.fields = c("from.fields(name,id)", "message", "story", "created_time", "type", "link"),
                         likes = TRUE,
                         likes.fields = c("id", "name"),
                         comments = TRUE,
                         comments.fields = c("id", "from.fields(name,id)" , "created_time", "like_count"),
                         reactions.summary = TRUE,
                         reactions.types = c("LIKE", "LOVE", "WOW", "HAHA", "SAD", "ANGRY", "THANKFUL")

){
  url <- sprintf(paste0("https://graph.facebook.com/%s?fields=",
                        paste(post.fields, collapse = ","),
                        ",shares.summary(true)",
                        ",comments.summary(true)",
                        ifelse(comments, sprintf(".fields(%s).limit(500)", paste(comments.fields, collapse = ",")), ""),
                        ",likes.summary(true)",
                        ifelse(likes, sprintf(".fields(%s).limit(2000)", paste(likes.fields, collapse = ",")),""),
                        ifelse(reactions.summary, ",reactions.fields(type)","")),
                 post.id)

  content <- tryCatch(callFBGraphAPI(url = url, token = token),
                      error = function(err) err)
  if (inherits(content, "error"))
    stop(sprintf("%s. Could not request FB Graph API.", content$message))
  #
  # error <- 0
  # while (length(content$error_code) > 0) {
  #   cat("Error!\n")
  #   Sys.sleep(0.5)
  #   error <- error + 1
  #   content <- callFBGraphAPI(url = url, token = token)
  #   if (error == 3) {
  #     stop(content$error_msg)
  #   }
  # }

  if ((l <- length(content)) == 0)
    stop("Post could not be found")

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

  attr(out, "post_id") <- post.id
  return(out)
}

# # test
# getPostData(post.id, token = fb_token)
# getPostData(post.id, token = fb_token, likes = FALSE, comments = FALSE)
