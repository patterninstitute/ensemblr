#The function for POST method
post <- function(res, ..., .headers = req_headers(), .body = NULL) {
  requests <- reqs(res, ..., .headers = .headers, .body = .body)
  responses <- purrr::map(requests, function(req) {
    req |> httr2::req_method("POST") |>
      httr2::req_perform()
  })
  # returns a list of responses, if multiple requests are made
  responses
}

# Retrieve the latest version for a set of Ensembl stable IDs, with a low level function
# https://rest.ensembl.org/documentation/info/archive_id_post
post_archive_ids <- function(ids, callback = NULL) {
  # Validate that the 'ids' parameter is provided and is a non-empty vector
  if (missing(ids) || length(ids) == 0) {
    stop("The 'ids' parameter is required and should contain at least one ID.")
  }

  # Create the body of the request in JSON format
  body <- jsonlite::toJSON(list(id = ids), auto_unbox = TRUE)

  # Make the POST request
  response <- post(
    res = "/archive/id",
    callback = callback,  # Optional query parameter for JSONP
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
