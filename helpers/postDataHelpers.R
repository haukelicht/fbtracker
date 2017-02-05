
#' @title Make Facebook post data frame.
#'
#' @description This helper function takes a list object representing
#'   post data requested from the Facebook Graph API,
#'   as returned by \code{\link{callFBGraphAPI}}, and returns a data frame.
#'
#' @param x A list object representing a facebook post as returned by \code{\link{callFBGraphAPI}}.
#'
#' @param post.fields A character vector,
#'   specifying the post fields that were requested
#'   (see \url{https://developers.facebook.com/docs/graph-api/reference/v2.8/post/}).
#'
#' @return A data frame object.
makePostDf <- function(x, post.fields)
{
  post_list_elmnts <- gsub("\\.fields(.*)$", "", post.fields)

  postData <- x[post_list_elmnts]

  null_entries <- which(vapply(postData, is.null, logical(1L)))
  if (any(null_entries)) {
    postData[null_entries] <- NA
    names(postData) <- post_list_elmnts
  }

  sublists <- which(vapply(postData, is.recursive, logical(1L)))

  sublist_names <- lapply(post.fields[sublists], function(sublist) {
    out <- gsub(pattern = "\\)$|.*\\.fields\\(", replacement = "", sublist)
    strsplit(out, ",")[[1]]
  })

  postData[sublists] <- lapply(postData[sublists], as.data.frame, stringAsFactor = FALSE)

  out <- do.call(cbind, postData)
  names(out) <- gsub("\\.", "_", names(out))
  if (any(idx <- grep("^type$|^link$", names(out)))) {
    names(out)[idx] <- paste0("post_", names(out)[idx])
  }
  
  likes_c <- postData$likes$summary$total_count
  cmnts_c <- postData$comments$summary$total_count
  shars_c <- postData$shares$count
  
  data.frame(out,
             likes_count = ifelse(is.null(likes_c),0L,likes_c),
             comments_count = ifelse(is.null(cmnts_c),0L,cmnts_c),
             shares_count = ifelse(is.null(shars_c),0L,shars_c),
             stringAsFactor = FALSE)
}

#' @title Make Facebook post likes data frame.
#'
#' @description This helper function takes a list object representing
#'   a post likes data requested from the Facebook Graph API,
#'   as returned by \code{\link{callFBGraphAPI}}, and returns a data frame.
#'
#' @details If \code{\link{callFBGraphAPI}} is used to request post data,
#'   likes data constitutes a sub-element of the post list object.
#'   By default this sub-element is named 'likes', so that \code{post$likes}
#'   can be passed to \code{makeLikesDf}'s first argument \code{x}.
#'
#' @param x A list object representing a facebook post likes edge.
#'
#' @param likes.fields A character vector, specifying the fields of the likes edge
#'   (see \url{https://developers.facebook.com/docs/graph-api/reference/v2.8/object/likes}).
#'
#' @return A data frame object.
#'   If no likes data available, an empty data frame is returned.
makeLikesDf <- function(x, likes.fields)
{
  if(length(x[["data"]]) == 0)
    return(data.frame(list()))

  likes_list_elmnts <- vector("character", length = 0L)

  with_fields <- grep("\\.fields(.*)$", likes.fields)

  j = 1
  for (i in seq_along(likes.fields)) {
    main <- gsub("\\..*", "", likes.fields[i])
    fields <- strsplit(gsub("^.*\\.fields\\(|\\)$", "", likes.fields[i]), ",")[[1]]
    if (identical(main, fields)) {
      likes_list_elmnts[j] <- main
      j <- j+1
    } else {
      likes_list_elmnts[j:(j+length(fields)-1)] <- paste(main, fields, sep = ".")
      j <- j+length(fields)
    }
  }

  out <- as.data.frame(do.call(rbind, lapply(x[["data"]], unlist)), stringsAsFactors = F)

  if (any(misng <- !(likes_list_elmnts %in% names(out)))) {
    out <- out[likes_list_elmnts[!misng]]
    l_out <- length(out)
    l <- sum(misng)
    out[l_out+1:l] <- as.list(rep(NA, l))
    names(out)[setdiff(seq_along(out), 1:l_out)] <- likes_list_elmnts[misng]
  } else {
    out <- out[likes_list_elmnts]
  }

  names(out) <- gsub("\\.", "_", names(out))

  out
}

#' @title Make Facebook post comments data frame.
#'
#' @description This helper function takes a list object representing
#'   a post comments data requested from the Facebook Graph API,
#'   as returned by \code{\link{callFBGraphAPI}}, and returns a data frame.
#'
#' @details If \code{\link{callFBGraphAPI}} is used to request post data,
#'   comments data constitutes a sub-element of the post list object.
#'   By default this sub-element is named 'comments', so that \code{post$comments}
#'   can be passed to \code{makeCommentsDf}'s first argument \code{x}.
#'
#' @param x A list object representing a facebook post comments edge.
#'
#' @param comments.fields A character vector, specifying the fields of the comments edge
#'   (see \url{https://developers.facebook.com/docs/graph-api/reference/v2.8/object/comments}).
#'
#' @return A data frame object.
#'   If no comments data available, an empty data frame is returned.
makeCommentsDf <- function(x, comments.fields)
{
  if(length(x[["data"]]) == 0)
    return(data.frame(list()))

  cmnts_list_elmnts <- vector("character", length = 0L)

  with_fields <- grep("\\.fields(.*)$", comments.fields)

  j = 1
  for (i in seq_along(comments.fields)) {
    main <- gsub("\\..*", "", comments.fields[i])
    fields <- strsplit(gsub("^.*\\.fields\\(|\\)$", "", comments.fields[i]), ",")[[1]]
    if (identical(main, fields)) {
      cmnts_list_elmnts[j] <- main
      j <- j+1
    } else {
      cmnts_list_elmnts[j:(j+length(fields)-1)] <- paste(main, fields, sep = ".")
      j <- j+length(fields)
    }
  }

  out <- as.data.frame(do.call(rbind, lapply(x[["data"]], unlist)), stringsAsFactors = F)

  if (any(misng <- !(cmnts_list_elmnts %in% names(out)))) {
    out <- out[cmnts_list_elmnts[!misng]]
    l_out <- length(out)
    l <- sum(misng)
    out[l_out+1:l] <- as.list(rep(NA, l))
    names(out)[setdiff(seq_along(out), 1:l_out)] <- cmnts_list_elmnts[misng]
  } else {
    out <- out[cmnts_list_elmnts]
  }

  names(out) <- gsub("\\.", "_", names(out))

  return(out)
}

#' @title Make Facebook post reactions summary.
#'
#' @description This helper function takes a list object representing
#'   a post data requested from the Facebook Graph API,
#'   as returned by \code{\link{callFBGraphAPI}}, and returns a vector of reactions
#'   summary statistics.
#'
#' @details If \code{\link{callFBGraphAPI}} is used to request post data,
#'   reactions data constitutes a sub-element of the post list object.
#'   By default this sub-element is named 'reactions', so that \code{post$comments}
#'   can be passed to \code{makeReactionsSummary}'s first argument \code{x}.
#'
#' @param x A list object representing a facebook post reactions edge.
#'
#' @param reactions.types A character vector, specifying the types of the reactions edge,
#'   (see \url{https://developers.facebook.com/docs/graph-api/reference/post/reactions}).
#'
#' @return A named integer vector,
#'   aggregating number of reactions aggregated by type and total.
#'   Vector element names correspond to type \*: 'react_\*_counts'
#'   If no reactions data available for a given type, count is 0.
makeReactionsSummary <- function(x, reactions.types)
{
  out <- as.data.frame(do.call(rbind, lapply(x[["data"]], unlist)), stringsAsFactors = F)

  counts <- vapply(split(out, out$type), nrow, integer(1L))[toupper(reactions.types)]
  counts[is.na(counts)] <- 0L

  names(counts) <- paste0("react_", tolower(reactions.types), "_counts")

  c(counts, "react_total_counts" = sum(counts))
}

