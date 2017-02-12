
#' @title Rearrange posts data from list of posts data lists
#'    into list of data frames.
#'
#' @description Posts data requested from the Facebook Graph API is returned as
#'   a list of lists of data frames by \code{\link{getPostsData}}, and thus 
#'   has an unconvenient, nested data structure.
#'   
#'   This function turns this structure into a list object 
#'   with data frame elements 'posts' 'post\_likes' and 'post\_comments', 
#'   so that all post data for all posts in nested list is in the respective 
#'   data frames and identified by 'post\_id'.
#'   
#' @param x A list opject of nested posts data.
#' 
#' @param likes Logical. Specifying whether the \code{x} contains posts' likes data.
#'   Defaults to \code{TRUE}.
#'     
#' @param likes Logical. Specifying whether to rearrange posts' likes data in \code{x}.
#'   Defaults to \code{TRUE}.
#'   
#' @param comments Logical. Specifying whether to rearrange posts' comments data in \code{x}.
#'   Defaults to \code{TRUE}.
#'
#' @param convert.timestamps Logical. If \code{TRUE}, data frames in input having column 
#'   'created\_time', Graph API-like timestamps are turned into conventional timestamps
#'   with format '%Y-%m-%d %H:%M:%S'. 
#'   Defaults to \code{TRUE}.
#'   
#' @param unescape.unicode Logical. If \code{TRUE}, \code{\link[stringi]{stri_unescape_unicode}} 
#'   is applied to data frames in input that have column 'message'. 
#'   Defaults to \code{TRUE}.
#'  
#' @param days.offset A positive integer scalar or \code{NULL}
#'   If not \code{NULL}, all data in input list data frames having column 'created\_time'
#'   that has a created time earlier than \code{days.offset} past from today is dropped.
#'   Defautls to \code{60L}
#'   
#' @return A list object of data frames and elements named 'posts' 'post\_likes' and 'post\_comments'.  
#' 
#' @importFrom stringi stri_unescape_unicode
rearrangePostsData <- function(x, 
                               likes = TRUE, 
                               comments = TRUE, 
                               convert.timestamps = TRUE, 
                               unescape.unicode = TRUE,
                               days.offset = 60L)
{
  stopifnot(is.list(x))
  if (!is.logical(likes))
    stop("'likes' must be logical.")
  
  if (!is.logical(comments))
    stop("'comments' must be logical.")
  
  # internal helpers
  assignSubelement <- function(x, name) {
    assign(x = name, value = lapply(x, `[[`, name), envir = parent.env(environment()))
  }
  
  rbindToDf <- function(x, 
                        unesc.unicode = unescape.unicode, 
                        convert.ts = convert.timestamps, 
                        offset = days.offset) 
  {
    out <- as.data.frame(do.call(rbind, x), stringsAsFactors = FALSE)
    out$post_id <- gsub("\\..*", "", rownames(out))
    rownames(out) <- NULL
    
    if ("message" %in% names(out))
      out$message <- stringi::stri_unescape_unicode(out$message)
    if ("created_time" %in% names(out) && convert.ts){
      out$created_time <- convertFBTimestamp(out$created_time, convert.to.date = FALSE)
      if (!is.null(offset))
        out <- out[format(as.POSIXlt(out$created_time), "%Y-%m-%d") >= (Sys.Date()-offset), ]
    }
      
    out
  }
  
  out <- list()
  
  assignSubelement(x, name = "post")
  Posts <- rbindToDf(post)

  assignSubelement(x, name = "timestamp")
  Timestamps <- rbindToDf(timestamp)
  
  out[["posts"]] <- merge(Posts, Timestamps, by = "post_id", all.x = TRUE)
  
  if (likes) {
    assignSubelement(x, name = "likes")
    Likes <- rbindToDf(likes)
    out[["post_likes"]] <- merge(Likes, Timestamps, by = "post_id", all.x = TRUE)
  }
  
  if (comments) {
    assignSubelement(x, name = "comments")
    Comments <- rbindToDf(comments)
    out[["post_comments"]] <- merge(Comments, Timestamps, by = "post_id", all.x = TRUE)
  }
  
  out
}
