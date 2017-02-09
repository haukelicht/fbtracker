

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
      
    
    out[]
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
