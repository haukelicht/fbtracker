
#' @title Update or insert posts data.  
#' 
#' @description Given a valid Facebook page ID, 
#'   function requests data retroperspectively for a given amount of (n) days
#'   for all posts posted on the page.
#'   It then compares the requested data to the posts data for this page stored
#'   in a PostgreSQL database, and inserts or updates posts data accordung to the 
#'   current storage.
#'   
#' @details The Function proceeds as follows.
#'   \itemize{
#'     \item[1.] page's post IDs for the last n days are queried, and compared
#'       to the post IDs in the database (DB) with created time within the last n days.
#'     \item[2.]{For the set of post IDs, that are already stored in the DB,
#'      \itemize{
#'        \item[(i)] Post data (optionally including posts' likes and comments data) 
#'          is requested from the Graph API and rearranged in a list of data frames, 
#'          containing posts data, and if requested, likes and comments data for all 
#'          post IDs in (2.).
#'        \item[(ii)] Post summary statistics as off query date (cf. the load timestamp)
#'          is written to the output list element 'post_data' for each post IDs in (2.).
#'        \item[(iii)] Post summary statistics as off query date (cf. the load timestamp)
#'          is written to the output list element 'post_data' for each post IDs in (2.).
#'      } 
#'     }
#'   
#'   } 
#'   Specifically,
#'   
#' @note Function is as of now tailored to interact with a specifc database structure.   
#' 
#' 
#' 
#' 
#' 


upsertPagePostsData <- function(page.id,
                                token = fb_token,
                                db.connection = con,
                                schema.name = "posts",
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
  where_clause <- sprintf("created_time::DATE >= '%s'::DATE AND from_id = '%s'",
                          Sys.Date()-days.offset, page.id)
  
  recorded_posts <- getSimpleQuery(conn = db.connection,
                                   select = "post_id", 
                                   from.table = "posts",
                                   from.schema = schema.name,
                                   where = where_clause)
  
  # determine existing posts (i.e., posts that are already recorded in DB)
  posts_in_db <- post_ids[post_ids$post_id %in% recorded_posts, "post_id"]
  
  if (length(posts_in_db) > 0){

    postsDataList <- tryCatch(getPostsData2(post.ids = posts_in_db, 
                                            token = token,
                                            post.fields = post.fields,
                                            likes = likes,
                                            likes.fields = likes.fields,
                                            comments = comments,
                                            comments.fields = comments.fields,
                                            reactions.summary = reactions.summary,
                                            reactions.types = reactions.types),
                              error = function(err) err)
    if (inherits(postsDataList, "error")){
      msg <- postsDataList$message
      return(list(error = msg, detail = "Could not update posts in DB.", post_ids = posts_in_db))    
    }
    
    postData <- rearrangePostsData(postsDataList, likes = likes, comments = comments, days.offset = NULL)
    
    # write post data only, because post is already recorded
    out$post_data <- postData$posts[, post.data.db.cols]
    
    where_this_page_id <- sprintf(#"load_timestamp::DATE >= '%s'::DATE AND 
      "regexp_replace(post_id, '_.*'::TEXT, ''::TEXT) LIKE '%s' AND post_id = ANY ('{%s}'::TEXT[])",
                                  #Sys.Date()-days.offset, 
      page.id,paste0(post_ids$post_id, collapse = ","))
    
    # Process Posts Likes
    if (likes && nrow(postData$post_likes) > 0) {
      ## check if posts like are recorded yet
      recorded <- getSimpleQuery(conn = db.connection,
                                 select = "post_id || '_' || user_id AS c_id, post_id, user_id", 
                                 from.table = "post_likes",
                                 from.schema = schema.name,
                                 where = where_this_page_id)  
      
      rmvd_recorded <- getSimpleQuery(conn = db.connection,
                                      select = "post_id || '_' || user_id AS c_id", 
                                      from.table = "post_likes_rmvd",
                                      from.schema = schema.name,
                                      where = where_this_page_id)  
      
      recorded_not_yet_rmvd <- setdiff(recorded$c_id, rmvd_recorded)
      
      valid_rec <- recorded[recorded$c_id %in% recorded_not_yet_rmvd, ]
      
      requested <- paste(postData$post_likes$post_id, postData$post_likes$user_id, sep = "_")
      
      ## write removed
      if (any(rmvd <- which(!valid_rec$c_id %in% requested))) {
        out[["post_likes_rmvd"]] <- as.data.frame(c(valid_rec[rmvd, 2:3], load_timestamp = ts()), stringsAsFactors = F)
      }

      ## write new
      if (any(new <- which(!requested %in% valid_rec$c_id))) {
        out$post_likes <- postData$post_likes[new, ]
      }
    }
    # Process Posts Comments
    if (comments && nrow(postData$post_comments) > 0) {
      ## check if posts comments are recorded yet
      recorded <- getSimpleQuery(conn = db.connection,
                                 select = "post_id || '_'::TEXT || cmnt_id as c_id, post_id, cmnt_id", 
                                 from.table = "post_comments",
                                 from.schema = schema.name,
                                 where = where_this_page_id)  
      
      rmvd_recorded <- getSimpleQuery(conn = db.connection,
                                      select = "post_id || '_'::TEXT || cmnt_id as c_id", 
                                      from.table = "post_comments_rmvd",
                                      from.schema = schema.name,
                                      where = where_this_page_id)  
      
      recorded_not_yet_rmvd <- setdiff(recorded$c_id, rmvd_recorded)
      
      valid_rec <- recorded[recorded$c_id %in% recorded_not_yet_rmvd, ]
      
      requested <- paste(postData$post_comments$post_id, postData$post_comments$cmnt_id, sep = "_")
      
      ## write removed
      if (any(rmvd <- which(!valid_rec$c_id %in% requested))){
        out[["post_comments_rmvd"]] <- as.data.frame(c(valid_rec[rmvd, 2:3], load_timestamp = ts()), stringsAsFactors = F)
      }
      
      ## write new
      if (any(new <- which(!requested %in% valid_rec$c_id))) {
        out$post_comments <- postData$post_comments[new, ]
      }
    }
  }
  
  posts_not_in_db <- post_ids[!post_ids$post_id %in% recorded_posts, "post_id"]
  
  if (length(posts_not_in_db) > 0){
    
    postsDataList <- tryCatch(getPostsData2(post.ids = posts_not_in_db, 
                                            token = token,
                                            post.fields = post.fields,
                                            likes = likes,
                                            likes.fields = likes.fields,
                                            comments = comments,
                                            comments.fields = comments.fields,
                                            reactions.summary = reactions.summary,
                                            reactions.types = reactions.types),
                              error = function(err) err)
    
    if (inherits(postsDataList, "error")){
      msg <- postsDataList$message
      return(list(error = msg, detail = "Could not add new posts to DB.", post_ids = posts_not_in_db))    
    }
    
    postData <- rearrangePostsData(postsDataList, likes = likes, comments = comments, days.offset = NULL)

    # write post, because post is not yet recorded
    out$posts <- rbind(out$posts, postData$posts[, posts.db.cols])
    out$post_data <- rbind(out$post_data, postData$posts[, post.data.db.cols])
    
    if (likes) {
      if ("post_likes" %in% names(out))
        out$post_likes <- rbind(out$post_likes, postData$post_likes)
      else
        out[["post_likes"]] <- postData$post_likes
    }
      
    if (comments) {
      if ("post_comments" %in% names(out))
        out$post_comments <- rbind(out$post_comments, postData$post_comments)
      else
        out[["post_comments"]] <- postData$post_comments
    }
  }
  
  attr(out, "page_id") <- page.id
  
  end <- Sys.time()
  
  attr(out, "run_time") <- rt <- format(round(end - start, 3), nsmall = 3)
  attr(out, "load_timestamp") <- ts()
  
  message(sprintf("Page ID '%s'. Total run time: %s", page.id, rt))

  return(out)
}
# 
# test <- upsertPagePostsData(page.id = "90022819050", token = fb_token, db.connection = con)
# str(test)
