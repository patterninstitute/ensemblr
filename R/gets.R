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
#'
#' @return A list of responses, one for each request made.
#'
#' @keywords internal
get <- function(res, ..., .headers = req_headers(), rate = 15/60) { # for get the body could be optional?
  requests <- reqs(res, ..., .headers = .headers)
  requests <- purrr::map(requests, httr2::req_throttle, rate = rate)
  responses <- httr2::req_perform_parallel(requests)
  httr2::throttle_status()

  #checking the rate limit exceptions and add delay if needed
  for (i in seq_along(responses)) {
    status_code <- httr2::resp_status(responses[[i]]) # I am not sure whether the error will arrive here
    if (status_code == 429) {
      #the `Retry-After` in the response_headers will only show up once you exceed the rate limit
      retry_after <- as.numeric(httr2::resp_headers(responses[[i]])$`Retry-After`)
      message(glue::glue("Rate limit reached, waiting {retry_after} seconds before retrying..."))
      Sys.sleep(retry_after)
      responses[[i]] <- httr2::req_perform(requests[[i]])
    } else if (status_code != 200) {
      warning(glue::glue("Request failed with status code {status_code}."))
    }
  }
  return(responses)
}

#----------------------------------------------------------------------------------------------
## example for the get, but for the user it will still require a bit more knowledge of the APIs
# response <- get("s/archive/id/{id}",
#                id = "ENSG00000139618",
#                type = "genomic",
#                species = "human")

# Retrieve a given Ensembl stable ID, with a low level function
# https://rest.ensembl.org/documentation/info/archive_id_get
get_archive_id <- function(id, callback = NULL) {
  if (missing(id)) { stop("The 'id' parameter is required.") }

  response <- get(
    res = "/archive/id/{id}",
    id = id,
    #callback = callback,  # optional query parameter, perferred not to use, ave to understand how it works
    .headers = req_headers(content_type = "application/json")
  )

  purrr::map(response, httr2::resp_body_json)
}

# # example
# result <- get_archive_id(id = "ENSG00000157764")
# print(result)
#
# WE CAN THEN GO ON WITH OTHER ENDPOINTS WITH GET METHOD
