
#' @description Build simple, i.e., non-joining sql query. 
#' @usage buildSimpleQuery <- function(conn = con, do.query = TRUE, select = "*", distinct = FALSE, distinct.on = NULL, from.schema = NULL, from.table, where = NULL, group.by = NULL, order.by = NULL, order.desc = TRUE, limit = NULL)
#' @param conn A connection object to an PostgreSQL database management system. 
#' Must be an \code{PostgreSQLConnection} object as produced by \code{RPostgreSQL::dbConnect}.
#' @param do.query A \code{boolean}, specifying wether built query should be get from database (default), 
#' or, if \code{FALSE}, only query statement should be returned.
#' @param select Columns to be selected \code{from.table} in \code{from.schema}.
#' @param distinct A boolean specifying wether to select distinct on result set. Default is \code{FALSE}.
#' @param distinct.on A character vector specifying the columns on which to \code{select distinct on}. 
#' Evaluated only,  yet optional if \code{distinct==TRUE}.
#' @param from.schema The schema from which to query data. 
#' Default is \code{NULL}. If \code{NULL}, query is sent to \code{current_schema()}.
#' @param from.table The Table from which to query data. No default.
#' @param where SQL where-clause. Default is \code{NULL}, and where-clause is ommitted in case.
#' @param group.by SQL groub-by clause. Regular SQL group-by rules apply. Default is \code{NULL}, implying no grouping.
#' @param order.by SQL order-by clause. Regular SQL order-by rules apply. Default is \code{NULL}, implying no ordering.
#' @param order.desc A boolean specifying whether to order data by order-by columns descendingly (if \code{TRUE}, the default), or ascendinlgy.
#' Evaluated only if \code{order.by} is not \code{NULL}.
#' @param limit A positiv integer limit specifying the maximum number of rows of result set returned.
#' @return If \code{do.query} is set to \code{TRUE}, a \code{data.frame} object of the result set of the built query. 
#' If \code{do.query} is set to \code{FALSE}, a \code{character} vector of the built query statment.
#' @import RPostgreSQL::dbGetQuery
buildSimpleQuery <- function(conn = con,
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
                             limit = NULL){
  
  stopifnot(class(conn) == "PostgreSQLConnection")
  required <- tryCatch(require(DBI), warning = function (w) w)
  if ("warning" %in% class(required)) stop(required$message)
  
  if (missing(from.table)) stop("No relation specified. `from` must be set!")
  
  includes_schema <- grepl("\\.", from.table)
  if (is.null(from.schema) && !includes_schema) {
    warning(sprintf("Schema no set. Will query relation in current schema '%s'.", psql("select current_schema")))
    from.schema <- dbGetQuery(conn = con, "select current_schema")[[1]]
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
  
  if (!do.query) return(statement)
  
  out <- tryCatch(dbGetQuery(conn = conn, statement),
                  error = function (e) e)
  
  if ("error" %in% class(out)) { stop(out$message) }
  
  out
} 
