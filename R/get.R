#' The function for the GET method
#'
#' The [get()] function is a wrapper around the [reqs()] function that performs
#' GET requests to the Ensembl API, handling rate limiting automatically.
#'
#' @param res The resource (path) for the API request, can include variables
#'   in curly braces `{}` that will be replaced with the corresponding parameter.
#' @param ... Additional named parameters to be included in the request URL.
#' @param .headers An S3 list with class `ensemblr_req_hdr`. Use the helper
#'   [req_headers()] to create such an object.
#' @param rate The maximum number of requests per second to allow.
#' Defaults to 15 per minute (15/60).
#' @param verbose Logical, if TRUE, enables detailed logging of request and response details.
#'
#' @return A list of responses, one for each request made.
#'
#' @keywords internal
get <- function(res, ..., .headers = req_headers(), rate = 15/60, verbose = FALSE) {
  requests <- reqs(res, ..., .headers = .headers)
  requests <- purrr::map(requests, httr2::req_throttle, rate = rate)

  responses <- vector("list", length(requests))
  for (i in seq_along(requests)) {
    if (verbose) message(glue::glue("Performing request {i}/{length(requests)}..."))
    repeat {
      response <- httr2::req_perform(requests[[i]])
      status_code <- httr2::resp_status(response)

      if (status_code == 429) {
        retry_after <- as.numeric(httr2::resp_headers(response)$`Retry-After`)
        if (is.na(retry_after)) {
          warning("Rate limit exceeded, but `Retry-After` header is missing. Defaulting to 60 seconds.")
          retry_after <- 60
        }
        message(glue::glue("Rate limit reached for request {i}, waiting {retry_after} seconds..."))
        Sys.sleep(retry_after)
      } else {
        if (status_code != 200) {
          warning(glue::glue("Request {i} failed with status code {status_code}."))
        }
        responses[[i]] <- response
        break
      }
    }
  }

  if (verbose) message("All requests completed.")
  return(responses)
}

