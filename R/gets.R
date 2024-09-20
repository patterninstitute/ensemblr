# The function for the GET method

get <- function(res, ..., .headers = req_headers()) { # for get the body could be optional?
  requests <- reqs(res, ..., .headers = .headers)
  responses <- httr2::req_perform_parallel(requests)
  return(responses)
}

## example for the get, but it will require a bit more knowledge of the APIs
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
