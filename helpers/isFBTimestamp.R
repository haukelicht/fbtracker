
#' @description Timestamps associated with facebook nodes and edges have a specific format. 
#' This function tests wether an input-vector has facebook's timestamp format
#' @usage isFBTimestamp(x)
#' @param x A postive-length vector of type character.
#' @return Boolean, evaluating to true, iff (all elements of) input vector is (are) formatted like '%Y/%m/%d %H:%M:%S0'
#' @example isFBTimestamp(x = '2016-08-31T13:30:12+0000')
isFBTimestamp <- function(x) {
  if (is.null(x)) return(NULL) 
  stdrd <- "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\+\\d{4}"
  all(vapply(x, function(d) grepl(stdrd, d), logical(1L)))
} 
