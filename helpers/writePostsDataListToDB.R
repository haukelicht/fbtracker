writePostsDataListToDB <- function(x, 
                                   conn,
                                   db.schema = "posts",
                                   db.relations = c("posts", 
                                                    "post_data", 
                                                    "post_likes", "post_likes_rmvd",
                                                    "post_comments", "post_comments_rmvd")
){
  # internal helpers
  assignSubelement <- function(x, name) {
    assign(x = name, value = lapply(x, `[[`, name), envir = parent.env(environment()))
  }
  rbindToDf <- function(x) {
    as.data.frame(do.call(rbind, x), stringsAsFactors = FALSE)
  }
  
  set_schema <- tryCatch(setCurrentSchema(conn = conn, db.schema), 
                         error = function(err) err)
  if (inherits(set_schema, "error"))
    stop("Could not set schema to '%s'. %s", db.schema, set_schema$message)
  
  for(rel in db.relations) {
    
    cols <- getSimpleQuery(conn, 
                           select = "column_name",
                           from.schema = "information_schema",
                           from.table = "columns",
                           where = sprintf("table_name LIKE '%s'", rel)) 
      
    assignSubelement(x, rel)
    df <- rbindToDf(get(rel))
    
    if (nrow(df) > 0) {
      sql_write <- DBI::sqlAppendTable(conn, rel, df[, cols], row.names = FALSE)
      DBI::dbSendQuery(conn, sql_write)
    }
  }
}
