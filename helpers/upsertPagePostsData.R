
upsertPagePostsData <- function(page.id = "100625493385",
                                token = fb_token,
                                db.connection = con,
                                days.offset = 60L,
                                post.fields = c("from.fields(name,id)", "message", "story", "created_time", "type", "link"),
                                likes = TRUE,
                                likes.fields = c("id", "name"),
                                comments = TRUE,
                                comments.fields = c("id", "from.fields(name,id)" , "created_time", "like_count"),
                                reactions.summary = TRUE,
                                reactions.types = c("LIKE", "LOVE", "WOW", "HAHA", "SAD", "ANGRY", "THANKFUL"),
                                posts.db.cols = c("post_id", "from_id", "created_time", "message", "post_type", "post_link", "load_timestamp"),
                                post.data.db.cols = c("post_id", "load_timestamp", "likes_count", "comments_count", "shares_count",
                                                     "react_like_counts", "react_love_counts", "react_wow_counts", "react_haha_counts", 
                                                     "react_sad_counts", "react_angry_counts", "react_thankful_counts", "react_total_counts")
){
  start <- Sys.time()
  
  out <- list(posts = data.frame(), post_data = data.frame())
  
  if (likes) 
    out[["post_likes"]] <- data.frame()
  
  if (comments) 
    out[["post_comments"]] <- data.frame()
  
  # test if page exists
  url <- sprintf("https://graph.facebook.com/%s?fields=id", page.id)
  
  page_exists <- callFBGraphAPI(url, token = fb_token, retry = 0L)
  
  if (inherits(page_exists, "error") || is.null(page_exists$id)){
    attr(out, "page_id") <- page.id
    
    end <- Sys.time()
    
    attr(out, "run_time") <- rt <- format(round(end - start, 3), nsmall = 3)
    
    attr(out, "error") <- "Page does not exist."
  }
  
  # get IDs and created_time of all post of page within the last 60 days
  post_ids <- getPostIDs(page.id, token, since = Sys.Date()-days.offset)
  
  # get IDs of all post recorded in DB of page within the last 60 days
  where_clause <- sprintf("created_time >= to_date('%s', 'yyyy-MM-dd') AND from_id LIKE '%s'",
                          Sys.Date()-days.offset, page.id)
  
  recorded_posts <- getSimpleQuery(conn = db.connection,
                                   select = "post_id", 
                                   from.table = "posts",
                                   from.schema = "posts",
                                   where = where_clause)
  
  # determine existing posts (i.e., posts that are already recorded in DB)
  posts_in_db <- post_ids[post_ids$post_id %in% recorded_posts, "post_id"]
  
  if (length(posts_in_db) > 0){
    
    postsDataList <- getPostsData(post.ids = posts_in_db, 
                                  token = token,
                                  post.fields = post.fields,
                                  likes = likes,
                                  likes.fields = likes.fields,
                                  comments = comments,
                                  comments.fields = comments.fields,
                                  reactions.summary = reactions.summary,
                                  reactions.types = reactions.types)  
    
    postData <- rearrangePostsData(postsDataList, likes = likes, comments = comments)
    # str(postData, 2)
    
    # write post data only, because post is already recorded
    out$post_data <- postData$posts[, post.data.db.cols]
    
    where_this_page_id = sprintf("regexp_replace(post_id, '_.*'::TEXT, ''::TEXT) LIKE '%s'", page.id)
    
    # Process Posts Likes
    if (likes) {
      ## check if posts like are recorded yet
      recorded <- getSimpleQuery(conn = db.connection,
                                 select = "post_id || '_' || user_id", 
                                 from.table = "post_likes",
                                 from.schema = "posts",
                                 where = where_this_page_id)  
      
      requested <- paste(postData$post_likes$post_id, postData$post_likes$id, sep = "_")
      
      ## write removed
      if (any(rmvd <- which(!recorded %in% requested))){
        out[["post_likes_rmvd"]] <- postData$post_likes[rmvd, ]
      }
      
      ## write new
      if (any(new <- which(!requested %in% recorded))) {
        # write 
        out$post_likes <- postData$post_likes[new, ]
      }
    }
    # Process Posts Comments
    if (comments) {
      ## check if posts comments are recorded yet
      recorded <- getSimpleQuery(conn = db.connection,
                                 select = "cmnt_id", 
                                 from.table = "post_comments",
                                 from.schema = "posts",
                                 where = where_this_page_id)  
      
      requested <- postData$post_comments$id
      
      ## write removed
      if (any(rmvd <- which(!recorded %in% requested))){
        out[["post_comments_rmvd"]] <- postData$post_likes[rmvd, ]
      }
      
      ## write new
      if (any(new <- which(!requested %in% recorded))) {
        # write 
        out$post_comments <- postData$post_likes[new, ]
      }
    }
  }
  
  posts_not_in_db <- post_ids[!post_ids$post_id %in% recorded_posts, "post_id"]
  
  if (length(posts_not_in_db) > 0){
    
    postsDataList <- getPostsData(post.ids = posts_not_in_db, 
                                  token = token,
                                  post.fields = post.fields,
                                  likes = likes,
                                  likes.fields = likes.fields,
                                  comments = comments,
                                  comments.fields = comments.fields,
                                  reactions.summary = reactions.summary,
                                  reactions.types = reactions.types)  
    
    postData <- rearrangePostsData(postsDataList, likes = likes, comments = comments)
    # str(postData, 2)
    
    # write post, because post is not yet recorded
    out$posts <- rbind(out$posts, postData$posts[, posts.db.cols])
    out$post_data <- rbind(out$post_data, postData$posts[, post.data.db.cols])
    
    if (likes) 
      out$post_likes <- rbind(out$post_likes, postData$post_likes)
    
    if (comments)
      out$post_comments <- rbind(out$post_comments, postData$post_comments)
  }
  
  attr(out, "page_id") <- page.id
  
  end <- Sys.time()
  
  attr(out, "run_time") <- rt <- format(round(end - start, 3), nsmall = 3)
  
  message(sprintf("Page ID '%s'. Total run time: %s", page.id, rt))

  return(out)
}

# test <- upsertPagePostsData("1522202731410785", fb_token, db.connection = con, days.offset = 60L)
# str(test,2)
