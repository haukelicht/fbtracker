
#' @description Run a Facebook Graph API request at \cite{url}, using \code{token}
#' @usage callFBGraphAPI(url, token)
#' @param url An URL of API request
#' @param url A temporary access token or an OAuth 2.0 token
callFBGraphAPI <- function (url, token) 
{
  req <- tryCatch(require(httr, quietly = TRUE), wraning = function (w) w, error = function (e) e)
  if (any(c("error","warning") %in% class(req))) stop(req$message)
  
  if (class(token)[1] == "config") {
    url.data <- httr::GET(url, config = token)
  }
  if (class(token)[1] == "Token2.0") {
    url.data <- httr::GET(url, config(token = token))
  }
  if (class(token)[1] == "character") {
    url <- paste0(url, "&access_token=", token)
    url <- gsub(" ", "%20", url)
    url.data <- httr::GET(url)
  }
  if (class(token)[1] != "character" & class(token)[1] != "config" & 
      class(token)[1] != "Token2.0") {
    stop("Error in access token. Must be a temporary or OAuth 2.0 token.")
  }
  content <- rjson::fromJSON(rawToChar(url.data$content))
  if (length(content$error) > 0) {
    stop(content$error$message)
  }
  return(content)
}
