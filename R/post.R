#' The function for POST method
#'
#' The [post()] function is a wrapper around the [reqs()] function that
#' performs POST requests to the Ensembl API, handling rate limiting
#' automatically.
#'
#' @param res The resource (path) for the API request, can include variables
#'   in curly braces `{}` that will be replaced with the corresponding
#'   parameter.
#' @param ... Additional named parameters to be included in the request URL.
#' @param .body The body of the POST request, can be a string or raw vector.
#' @param .headers An S3 list with class `ensemblr_req_hdr`. Use the helper
#'   [req_headers()] to create such an object.
#' @param rate The maximum number of requests per second to allow.
#' on a request (default: 5).
#' Defaults to 15 per minute (15/60).
#'
#' @return A list of responses, one for each request made.
#'
#' @keywords internal
post <- function(res, ..., .headers = req_headers(), .body, rate = 15/60) { # for post the body could be mandatory?
  if (missing(.body)) stop("The '.body' parameter is required for POST requests.")
  requests <- reqs(res, ..., .headers = .headers, .body = .body)
  requests <- purrr::map(requests, httr2::req_throttle, rate = rate)
  responses <- purrr::map(requests, function(req) {
    req |> httr2::req_method("POST") |>
      httr2::req_perform()
  })
  httr2::throttle_status()

  for (i in seq_along(responses)) {
    status_code <- httr2::resp_status(responses[[i]]) # I am not sure whether the error will arrive here
    if (status_code == 429) {
      #the `Retry-After` in the response_headers will only show up once you exceed the rate limit
      retry_after <- as.numeric(httr2::resp_headers(responses[[i]])$`Retry-After`)
      message(glue::glue("Rate limit reached, waiting {retry_after} seconds
                         before retrying..."))
      Sys.sleep(retry_after)
      responses[[i]] <- httr2::req_perform(requests[[i]])
    } else if (status_code != 200) {
      warning(glue::glue("Request failed with status code {status_code}."))
    }
  }

  responses
}

#--------------------------------------------------------------------------------------
# Retrieve the latest version for a set of Ensembl stable IDs, with a low level function
# https://rest.ensembl.org/documentation/info/archive_id_post
post_archive_ids <- function(ids, callback = NULL) {
  if (missing(ids) || length(ids) == 0) {
    stop("The 'ids' parameter is required and should contain at least one ID.")
  }

  body <- jsonlite::toJSON(list(id = ids), auto_unbox = TRUE)

  response <- post(
    res = "/archive/id",
    callback = callback,  # optional query parameter for JSONP
    .headers = req_headers(content_type = "application/json"),
    .body = body
  )
}

# #example
# result <- post_archive_ids(ids = c("ENSG00000157764", "ENSG00000248378"),
#                           callback = "myCallbackFunction")
# print(result)

# WE CAN THEN GO ON WITH OTHER ENDPOINTS WITH POST METHOD
# {the rest of the endpoints functions are in file `ensembl-endpoins.R`}
