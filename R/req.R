vars_in_braces <- function(x) {
  matches <- stringr::str_extract_all(x, "\\{([^}]+)\\}")
  vars <- gsub("[{}]", "", unlist(matches))

  vars
}

# Helper functions for http headers
# The reference to which headers parameters were taken:
# https://github.com/Ensembl/ensembl-rest/wiki/HTTP-Headers

request_headers <-
  function(accept = NULL,
           accept_encoding = NULL,
           content_type =  "application/json",
           origin = NULL) {
    .headers <- list(
      accept = accept,
      accept_encoding = accept_encoding,
      content_type = content_type,
      origin = origin
    )

    .headers <-
      .headers[!sapply(.headers, is.null)] # this will remove NULL entries

    structure(.headers, class = "request_headers")
  }

response_headers <-
  function(access_control_allow_origin = "*",
           content_length = NULL,
           content_type = "text/x-fasta",
           retry_after = NULL,
           x_runtime = NULL,
           x_rate_limit_limit = NULL,
           x_rate_limit_reset = NULL,
           x_rate_limit_period = NULL,
           x_rate_limit_remaining = NULL) {

    ..headers <- list(
      access_control_allow_origin = access_control_allow_origin,
      content_length = content_length,
      content_type = content_type,
      retry_after = retry_after,
      x_runtime = x_runtime,
      x_rate_limit_limit = x_rate_limit_limit,
      x_rate_limit_reset = x_rate_limit_reset,
      x_rate_limit_period = x_rate_limit_period,
      x_rate_limit_remaining = x_rate_limit_remaining
    )

    ..headers <-
      ..headers[!sapply(..headers, is.null)]

    structure(..headers, class = "response_headers")
  }


base_url <- function() "https://rest.ensembl.org"

user_agent <- function() "ensemblr (https://www.pattern.institute/ensemblr)"

# Example:
#
# req("/cafe/genetree/member/symbol/{species}/{symbol}", species = "homo_sapiens", symbol = "BRCA2") |>
#   httr2::req_perform() |>
#   httr2::resp_body_json()
req <-
  function(res,
           ...,
           .body = NULL,
           .headers = request_headers()) {

    # All parameters.
    params <- list(...)
    pnames <- names(params)

    # Required parameters.
    req_pnames <- vars_in_braces(res)
    req_params <- params[req_pnames]

    # Optional parameters.
    opt_pnames <- setdiff(pnames, req_pnames)
    opt_params <- params[opt_pnames]

    res <- glue::glue(res, .envir = as.environment(req_params))

    req <-
      httr2::request(base_url()) |>
      httr2::req_url_path_append(res) |>
      httr2::req_url_query(!!!opt_params) |>
      httr2::req_headers(
        Accept = .headers$accept,
        `Accept-Encoding` = .headers$accept_encoding,
        `Content-Type` = .headers$content_type,
        Origin = .headers$origin
      ) |>
      httr2::req_user_agent(user_agent())

    if (!is.null(.body)) {
      req <- httr2::req_body_raw(req, body = .body, type = .headers$content_type)
    }

    req
  }

reqs <- function(res,
                 ...,
                 .body = NULL,
                 .headers = request_headers()) {
  params <- list(...)
  .body <- .body %||% list(.body)

  req_args <-
    vctrs::vec_recycle_common(
      res = res,
      !!!params,
      .body = .body,
      .type = .headers$content_type
    )
  reqs <- purrr::pmap(.l = req_args, .f = req)

  reqs
}
