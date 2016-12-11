
#' @description Function checks if input is date or has date format, and returns formatted date 
#' @param x unit-length object to be tested 
#' @param use.format date-format to be used for output. Default is '%Y/%m/%d'
#' @return date-formatted as specified in \code{use.format}  
ifDateInput <- function(x, use.format = "%Y/%m/%d") 
{
  input_is_Date <- any(class(x) %in% c("POSIXt", "Date"))
  input_is_character <- ifelse(!input_is_Date, typeof(x) == "character", NA)
  
  if(input_is_Date) {
    return(x)
  } else if (input_is_character) {
    if (!grepl("\\d{4}/\\d{2}/\\d{2}", x)) stop("Method not applicable if input to `%s` violates 'YYYY/mm/dd' date format.")
    return(as.Date(x, format = use.format))
  } else {
    stop("Method not applicable if class of input to `%s` is neither 'Date' nor 'character'.")
  }
}