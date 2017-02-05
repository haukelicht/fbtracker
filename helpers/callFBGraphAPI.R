
#' @title Call Facebook Graph API.
#'
#' @description Send a Facebook Graph API request to \code{url}, using
#'   \code{token}.
#'
#' @details \code{token} must be either an temporary Facebook access token,
#'   generated at \url{developers.facebook.com/tools/explorer/},
#'   and passed as an unit-length character vector; or an
#'   object of class 'OAuth 2.0'.
#'
#'   Note that for each \code{retry}, \code{callFBGraphAPI} is called
#'   recursively with a sleep time of a quarter second.
#'   Increasing the number of retries thus may substantially increase
#'   the run time of your request.
#'
#' @param url A unit-length character vector, specifying the URL request.
#'
#' @param token A temporary access token or an OAuth 2.0 token object (see
#'   details).
#'
#' @param retry A positive integer value, specifying the number of retries
#'   when initial request fails with status different from 200.
#'  ?str
#' @return A list object, with list structure corresponding to JSON returned by
#' Facebook Graph API.
#'
#' @import
#'   httr GET,
#'   httr config,
#'   httr status_code,
#'   rjson fromJSON
#'
callFBGraphAPI <- function (url, token, retry = 3L)
{
  # catchIt(suppressPackageStartupMessages(require(httr)), "condition", stop)
  # catchIt(suppressPackageStartupMessages(require(rjson)), "condition", stop)

  if (!is.character(url))
    stop("'url' must be a http adresse, passed as an atomic unit-length character string.")

  # helper
  buildError <- function(error.class, message, ..., inherit.class = c("error", "condition"))
  {
    structure(class = c(error.class, inherit.class), list(message = message, ...))
  }

  if (class(token)[1] == "config") {
    urlData <- httr::GET(url, config = token)
  } else if (class(token)[1] == "Token2.0") {
    urlData <- httr::GET(url, httr::config(token = token))
  } else if (class(token)[1] == "character") {
    url <- paste0(url, "&access_token=", token)
    url <- gsub(" ", "%20", url)
    urlData <- httr::GET(url)
  } else {
    msg <- "Error in access token. Must be a temporary or OAuth 2.0 token."
    return(buildError("InvalidTokenError", message = msg))
  }

  status <- httr::status_code(urlData)

  if (status != 200L) {
    msg <- sprintf("Url call failed with status '%s'", httr::http_status(urlData)$message)
    return(buildError("httpStatusError", message = msg, status = status))
  }

  content <- rjson::fromJSON(rawToChar(urlData$content))

  if (length(content$error_code) > 0) {

    error <- 0L
    while (length(content$error_code) > 0) {

      Sys.sleep(0.25)
      error <- error + 1L
      content <- tryCatch(callFBGraphAPI(url = url, token = token, retry = 0L),
                          error = function(e) e)
      if (inherits(content, "error"))
        return(content)

      if (error == retry) {
        return(buildError("GraphAPIError",
                          message = content$error_msg,
                          error_code = content$error_code))
      }
    }
  }

  return(content)
}

