
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

