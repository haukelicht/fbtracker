
#' @title Update or insert posts data.  
#' 
#' @description Given a valid Facebook page ID, 
#'   function requests data retroperspectively for all posts posted 
#'   on the page within the last \emph{n} days, 
#'   including post reactions summary, likes, and comments, if respective parameters are set to \code{TRUE}.
#'   It then compares the requested data to the posts data for this page stored
#'   in a PostgreSQL database, and inserts or updates posts data according to the 
#'   current storage.
#'   
#' @param page.id A Facebook Graph API page ID.
#' 
#' @param token A valid Facebook Graph API access token.
#' 
#' @param db.connection A valid 'PostgreSQL' database connection object.
#' 
#' @param schema.name A unit-length character vector, 
#'   specifying the schema to query DB data from.
#'   Defaults to schema 'posts'.
#'   
#' @param days.offset An integer scalar, specifying the number of days for which 
#'   to request, and insert and/or update DB posts data.
#'   Defaults to \code{60L}, i.e., sixty days past from today.
#'   
#' @param post.fields A character vector, matching the fields of the Graph API post node.
#'   Defaults to \code{c("from.fields(name,id)", "message", "story", "created_time", "type", "link")}, 
#'   i.e., post author name and ID, post message, story, creation timestamp and type, and a link, 
#'   if applies. 
#'   
#' @param likes Logical. Specifying whether to request posts' likes data.
#'   Defaults to \code{TRUE}.
#'   
#' @param likes.fields A character vector, matching the fields of the Graph API likes edge.
#'   By default fields 'id' and 'name' (user ID and name) are requested.
#' 
#' @param comments Logical. Specifying whether to request posts' comments data.
#'   Defaults to \code{TRUE}.
#'   
#' @param comments.fields A character vector, matching the fields of the Graph API comments edge.
#'   Defaults to \code{c("id", "from.fields(name,id)" , "created_time", "like_count")},
#'   i.e., comment ID, comment author name and ID, comment creation timestamp 
#'   and the count of comment likes as off time of request.
#'   
#' @param reactions.summary Logical. Specifying whether to request posts' reactions data.
#'   Defaults to \code{TRUE}.
#' 
#' @param reactions.types A character vector, matching the reactions types specified 
#'   in the Graph API reactions field.
#'   Defaults to types 'LIKE', 'LOVE', 'WOW', 'HAHA', 'SAD', 'ANGRY', and 'THANKFUL'.
#' 
#' @param posts.db.cols A character vector, matching the column names of the DB table 'posts'.
#'   Defautls to 'post\_id', 'from\_id', 'created\_time', 'message', 'post\_type', 
#'   'post\_link', and 'load\_timestamp'.
#' 
#' @param post.data.db.cols A character vector, matching the column names of the DB table 'post\_data'.
#'   Defaults to 'post\_id', 'load\_timestamp', 'likes\_count', 'comments\_count', 'shares\_count',
#'   'react\_like\_counts', 'react\_love\_counts', 'react\_wow\_counts', 'react\_haha\_counts', 
#'   'react\_sad\_counts', 'react\_angry\_counts', 'react\_thankful\_counts', and 'react\_total\_counts'.
#'                 
#' @details The Function proceeds as follows.
#'   \itemize{
#'     \item[1.] page's post IDs for the \emph{n} days between today 
#'       and \code{days.offset} before today are queried, and compared
#'       to the post IDs in the database (DB) with created time within the last \emph{n} days.
#'     \item[2.]{For the set of post IDs, that are already stored in the DB,
#'       \itemize{
#'        \item[(i)] Post data is requested from the Graph API, and rearranged 
#'          in a list of data frames, containing posts reactions summary 
#'          statistics data, and if requested, likes and comments data for all 
#'          post IDs in (2.).
#'        \item[(ii)] Post summary statistics as off query date (cf. the load timestamp)
#'          are written to the output list element 'post\_data' with load timestamp
#'          for each post IDs in (2.).
#'        \item[(iii)]{If post likes are requested, too, queried user likes
#'          are compared to the likes already stored in tables 'post\_likes' and 
#'          'post\_likes\_rmvd' (removed post likes) in the DB for each post ID in (2.)
#'            \itemize{
#'              \item[(a)] If queried post ID-user ID entry is \emph{not} in the DB
#'                (i.e. exists neither in 'post\_likes', nor in 'post\_likes\_rmvd'),
#'                like data entry is wirtten to list output 'post\_likes' with load timestamp.
#'              \item[(b)] If queried post ID-user ID entry exists in DB, 
#'                no action is taken.
#'              \item[(b)] If a post ID-user ID entry exists in 'post\_likes' in the DB,
#'                but is not in the queried data, it is written to list output 
#'                'post\_likes\_rmvd' with load timestamp.
#'            }
#'        }
#'        \item[(iv)]A similar procedure as described in (iii) is repeated 
#'          for post comments, if post comments data is queried, too.
#'          The only minor difference is, that comments are uniquely identified 
#'          by post ID-cmnt ID combinations, not necessarily by post and user ID,
#'          for one users can comment multiple posts multiple times.  
#'      } 
#'     }
#'     \item[3.]{For the set of post IDs, that are not yet stored in the DB, that is, 
#'       that are in the set difference of the two queries from (1.), 
#'       \itemize{
#'         \item[(i)] Post data is requested from the Graph API, and rearranged 
#'          in a list of data frames, containing posts reactions summary 
#'          statistics data, and if requested, likes and comments data for all 
#'          post IDs in (3.).
#'        \item[(ii)] All data is immediately appendend to the respective elements
#'          of the output list (including load timestamp), because writing 'new'
#'          (i.e. newly tracked) posts to the DB requires no comparison to existsing data.
#'      }
#'     }
#'     \item[4.] Attributes 'page\_id' (Pade ID for which posts data is uperseted), 
#'       'load\_timetamp' (general timestamp of API request), 
#'       'run\_time' (total run time of function call for page ID), and 
#'       are assigned to the output list, and the list object is returned. 
#'   } 
#'   
#' @note Function is tailored to interact with a specifc database structure, defined
#'   in jobs/setup_db.sql.
#' 
#' @return A list of data frames, with elements named 'posts' and 'post\_data'. 
#'   If \code{likes} respectively \code{comments} is/are set to \code{TRUE},
#'   data frames named 'post\_likes' and  'post\_comments' are added, too, 
#'   and, if applies, 'post\_likes\_rmvd' and  'post\_comments\_rmvd'.
#'   
#'   In addition, the list has attrubites 'page\_id', 'run\_time', and 'load\_timestamp'.

upsertPagePostsData <- function(page.id,
                                token,
                                db.connection,
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
    
    where_this_page_id <- sprintf(
      "regexp_replace(post_id, '_.*'::TEXT, ''::TEXT) LIKE '%s' AND post_id = ANY ('{%s}'::TEXT[])",
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
