
#' @title Get simple SQL query.
#' 
#' @description Get result set of a simple, that is, non-joining SQL query. 
#' 
#' @param conn A connection object to a SQL database management system.
#'   
#' @param do.query Logical. If \code{TRUE}, query is actually sent to \code{conn}.
#'  Otherwise, query statement is returned as unit-length character vector.
#'  
#' @param select Unit-lengt characer vector. 
#'   Columns to be selected \code{from.table} in \code{from.schema}.
#'   Defaults to "*", querying all columns.
#'   
#' @param distinct Logical, specifying whether to select distinct on result set. 
#'   Defaults to \code{FALSE}.
#'   
#' @param distinct.on A character vector specifying the columns on 
#'   which to \code{select distinct}. 
#'   Evaluated only, yet optionally if \code{distinct=TRUE}.
#'   
#' @param from.schema  Unit-lengt characer vector. The schema from which to query data. 
#'   Defaults to \code{NULL}. 
#'   If \code{NULL}, query is sent to current schema on \code{conn}.
#'   
#' @param from.table Unit-lengt characer vector. 
#'   The table from which to query data.
#' 
#' @param where Unit-lengt characer vector. SQL where-clause. 
#'   Defaults to \code{NULL}, and where-clause is ommitted in case.
#' 
#' @param group.by  Unit-lengt characer vector. SQL groub-by clause. 
#'   Regular SQL group-by rules apply. 
#'   Defaults to \code{NULL}, i.e., no grouping.
#'   
#' @param order.by  Unit-lengt characer vector. SQL order-by clause. 
#'   Regular SQL order-by rules apply. 
#'   Defaults to \code{NULL}, i.e., no ordering.
#'   
#' @param order.desc Logical, specifying whether to order data 
#'   by order-by columns descendingly (if \code{TRUE}, the default), or ascendingly.
#'   Evaluated only if \code{order.by} is not \code{NULL}.
#'   
#' @param limit A positiv integer, 
#'   specifying the maximum number of rows of result set returned.
#'   
#' @returns If \code{do.query} is set to \code{TRUE}, a data frame 
#'   object of the result set of the built query.
#'   If \code{do.query} is set to \code{FALSE}, 
#'   the query statment as unit-length character vector
#'   
#' @importFROM DBI dbGetQuery
getSimpleQuery <- function(conn,
                           do.query = TRUE,
                           select = "*",
                           distinct = FALSE,
                           distinct.on = NULL,
                           from.schema = NULL,
                           from.table,
                           where = NULL,
                           group.by = NULL,
                           order.by = NULL,
                           order.desc = TRUE,
                           limit = NULL,
                           simplify = TRUE){
  
  # stopifnot(class(conn) == "PostgreSQLConnection")

  if (missing(from.table)) stop("No table specified to query from. `from` must be set!")
  
  includes_schema <- grepl("\\.", from.table)
  if (is.null(from.schema) && !includes_schema) {
    message(sprintf("Schema no set. Will query relation in current schema '%s'.", 
                    psql("select current_schema", conn = conn)))
    from.schema <- DBI::dbGetQuery(conn = conn, "select current_schema")[[1]]
  }

  select_query <- paste(ifelse(distinct,"DISTINCT",""),
                        ifelse(is.null(distinct.on),"", sprintf("ON (%s)",paste(distinct.on, collapse = ", "))),
                        select)
    
  statement <- paste("SELECT", select_query, 
                     "FROM", ifelse(includes_schema, from.table, paste(from.schema, from.table, sep = ".")), 
                     ifelse(is.null(where), "", paste("WHERE", where)), 
                     ifelse(is.null(group.by), "", paste("GROUP BY", group.by)), 
                     ifelse(is.null(order.by), "", paste("ORDER BY", order.by)), 
                     ifelse(is.null(order.by), "", ifelse(order.desc, "desc", "asc")),
                     ifelse(is.null(limit), "", paste("LIMIT", as.character(limit)))) 

  statement <- gsub("\\s+", " ", statement)
  
  if (!do.query) 
    return(statement)
  
  out <- tryCatch(DBI::dbGetQuery(conn = conn, statement),
                  error = function (err) err)
  
  if (inherits(out, "error"))
    stop(out$message)
  
  if (simplify && !is.null(out) && ncol(out) == 1L) 
    return(as.vector(out[, 1])) 
  else if (simplify && !is.null(out) && ncol(out) == 0L) 
    return(character(0L)) 
  else 
    out
} 
