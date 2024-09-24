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
#' Defaults to 15 per minute (15/60).
#'
#' @return A list of responses, one for each request made.
#'
#' @keywords internal
post <- function(res, ..., .headers = req_headers(), .body, rate = 15/60) { # for post the body could be mandatory?
  requests <- reqs(res, ..., .headers = .headers, .body = .body)
  responses <- purrr::map(requests, function(req) {
    req |> httr2::req_method("POST") |>
      httr2::req_perform()
  })
  for (i in seq_along(responses)) {
    if (httr2::resp_status(responses[[i]]) == 429) {
      retry_after <- as.numeric(httr2::resp_headers(responses[[i]])$`Retry-After`)
      message(glue::glue("Rate limit reached, waiting {retry_after} seconds
                         before retrying..."))
      Sys.sleep(retry_after)
      responses[[i]] <- httr2::req_perform(requests[[i]])
    }
  }

  # returns a list of responses, if multiple requests are made
  responses
}

#--------------------------------------------------------------------------------------
# Retrieve the latest version for a set of Ensembl stable IDs, with a low level function
# https://rest.ensembl.org/documentation/info/archive_id_post
post_archive_ids <- function(ids, callback = NULL) {
  # Validate that the 'ids' parameter is provided and is a non-empty vector
  if (missing(ids) || length(ids) == 0) {
    stop("The 'ids' parameter is required and should contain at least one ID.")
  }

  # create the body of the request in JSON format
  body <- jsonlite::toJSON(list(id = ids), auto_unbox = TRUE)

  # POST request
  response <- post(
    res = "/archive/id",
    callback = callback,  # optional query parameter for JSONP
    .headers = req_headers(content_type = "application/json"),
    .body = body
  )

  # Parse and return the response as JSON
  purrr::map(response, httr2::resp_body_json)
}

# #example
# result <- post_archive_ids(ids = c("ENSG00000157764", "ENSG00000248378"),
#                           callback = "myCallbackFunction")
# print(result)
#
# WE CAN THEN GO ON WITH OTHER ENDPOINTS WITH POST METHOD
