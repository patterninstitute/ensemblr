#' Ensembl REST API server
#'
#' @return A string containing Ensembl REST API server URL.
#' @keywords internal
ensembl_server <- function() "https://rest.ensembl.org"

#' User agent identification
#'
#' Generates an S3 \code{request} object as defined by the package \code{httr},
#' that is used to identify this package as the user agent in requests to the
#' Ensembl REST API. The user agent identification string is: \code{"ensemblr: R
#' Client for the Ensembl REST API"}.
#'
#' @return An S3 \code{request} object as defined by the package \code{httr}.
#' @keywords internal
user_agent <- function() {
  httr::user_agent("ensemblr (https://www.pattern.institute/ensemblr)")
}

#' Warn if response errored
#'
#' Warn if an httr \code{\link[httr]{response}} errored. It also returns a tidy
#' warning message.
#'
#' @param response A \code{\link[httr]{response}} object.
#'
#' @return A scalar character vector with a warning message, or the string
#'   \code{'OK'} if the response was successful, although this function is
#'   called mostly for its side effect, i.e., the triggering of a warning.
#' @keywords internal
warn_when_request_errored <- function(response) {
  code <- httr::status_code(response)
  # If status code is 200 (sucessful) then there is nothing to be done in this
  # function.
  if (identical(code, 200L)) {
    return("OK")
  }

  url <- response$url
  type <- httr::http_type(response)
  content <- httr::content(response, "text", encoding = "UTF-8")

  if (identical(type, "application/json")) {
    response_msg <- (jsonlite::fromJSON(content, flatten = TRUE))$error
  } else {
    content2 <- httr::content(response, as = "parsed", encoding = "UTF-8")
    if (identical(code, 503L)) {
      msg1 <- rvest::html_text(rvest::html_nodes(content2, "body"), trim = TRUE)
      msg2 <- stringr::str_replace(msg1, "\\n", "\\. ")
      response_msg <- glue::glue("{msg2}")
    } else {
      # NB rvest::html_nodes uses only one of the two arguments: css or xpath.
      # That is why xpath being missing from the call below is not an issue.
      msg1 <- rvest::html_text(rvest::html_nodes(content2, "h1"), trim = TRUE)
      msg2 <- rvest::html_text(rvest::html_nodes(content2, "h2"), trim = TRUE)
      response_msg <- glue::glue("{msg1}. {msg2}.")
    }
    # TODO: handle 400 error, e.g. resource_url = '/info/variation/populations/homo_sapiens/little humans'
  }

  wrn_msg <- glue::glue(
    "\n\n",
    "* Status code:    {code}\n",
    "* Server message: {response_msg}\n",
    "* Endpoint:       {url}"
  )

  warning(wrn_msg, immediate. = TRUE, call. = FALSE)
  return(wrn_msg)
}

#' Request an endpoint from Ensembl REST API
#'
#' Performs a \code{\link[httr]{GET}} request on the endpoint as specified by
#' \code{resource_url}.
#'
#' @param resource_url Endpoint URL. The endpoint is internally appended to the
#'   \code{base_url}. It should start with a forward slash (\code{'/'}).
#' @param base_url The Ensembl REST API base URL.
#' @param verbose Whether to be verbose.
#' @param warnings Whether to print warnings.
#'
#' @return A named list of four elements:
#' \describe{
#' \item{url}{The URL endpoint.}
#' \item{response_code}{\href{https://tinyurl.com/8yqvhwf}{HTTP
#' status code}.}
#' \item{status}{A string describing the status of the response obtained:
#' \code{"OK"} if successful or a description the error.}
#' \item{content}{The parsed JSON as a nested list, as returned by
#' \code{\link[jsonlite]{fromJSON}}.}
#' }
#'
#' @keywords internal
request <- function(resource_url, base_url = ensembl_server(),
                    verbose = FALSE, warnings = TRUE) {
  if (verbose) message(glue::glue("Base URL: {base_url}."))

  url <- stringr::str_c(base_url, resource_url)
  if (verbose) message(glue::glue("Requesting resource: {url}."))

  if (verbose) message(glue::glue("Using the user agent: {user_agent_id()$options$useragent}."))
  response <- httr::GET(url, user_agent())

  response_code <- httr::status_code(response)
  if (verbose) message(glue::glue("Response code: {response_code}."))

  # Response object (a list of four elements):
  #   - url: the resource URL
  #   - response_code: response code
  #   - status: short remark about what went wrong with the response,
  #     or OK if successful.
  #   - response content: the content of the parsed JSON response, or NULL if
  #     not successful.
  obj <- list(
    url = url,
    response_code = response_code,
    status = NA_character_,
    content = NULL
  )

  # If response is not 200, i.e. if request did not complete successfully then
  # return an empty response object (NULL) and warn about the response code.
  if (!identical(response_code, 200L)) {
    wrg_msg <- warn_when_request_errored(response)
    obj$status <- wrg_msg
    return(obj)
  } else { # Else response code is 200 and we move on to JSON parsing.

    # Check if the content type of the response is JSON.
    content_type <- httr::http_type(response)
    if (verbose) message(glue::glue("Response content type: {content_type}."))

    if (!identical(content_type, "application/json")) {
      if (warnings) {
        wrg_msg <- glue::glue("Response to {url} did not return JSON!")
        warning(wrg_msg, immediate. = TRUE, call. = FALSE)
      }
      obj$status <- "Response content was not application/json."
      return(obj)
    }

    # Parse JSON content
    content <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = FALSE)

    obj$status <- "OK"
    obj$content <- content
    return(obj)
  }
}

#' Parallel version of request
#'
#' Performs a \code{\link[httr]{GET}} request on each of the endpoints as
#' specified by \code{resource_urls}.
#'
#' @param resource_urls Vector of endpoint URLs. Each endpoint is internally
#'   appended to the \code{base_url}. It should start with a forward slash
#'   (\code{'/'}).
#' @param verbose Whether to be verbose.
#' @param warnings Whether to print warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A list of named lists of four elements:
#' \describe{
#' \item{url}{The URL endpoint.}
#' \item{response_code}{\href{https://tinyurl.com/8yqvhwf}{HTTP
#' status code}.}
#' \item{status}{A string describing the status of the response obtained:
#' \code{"OK"} if successful or a description the error.}
#' \item{content}{The parsed JSON as a nested list, as returned by
#' \code{\link[jsonlite]{fromJSON}}.}
#' }
#'
#' @keywords internal
request_parallel <- function(resource_urls,
                             verbose = FALSE,
                             warnings = TRUE,
                             progress_bar = TRUE) {
  # Usually we'd use purrr::map here but we opted for plyr::llply
  # for a no frills alternative with progress bar support.
  progress <- dplyr::if_else(progress_bar && interactive(), "text", "none")
  responses <- plyr::llply(
    .data = resource_urls,
    .fun = request,
    verbose = verbose,
    warnings = warnings,
    .progress = progress
  )

  return(responses)
}
