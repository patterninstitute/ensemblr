vars_in_braces <- function(x) {
  matches <- stringr::str_extract_all(x, "\\{([^}]+)\\}")
  vars <- gsub("[{}]", "", unlist(matches))

  vars
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
           .type = 'application/json') {

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
      httr2::req_headers(Accept = .type) |>
      httr2::req_user_agent(user_agent())

    if (!is.null(.body)) {
      req <- httr2::req_body_raw(req, body = .body, type = .type)
    }

    req
  }

reqs <- function(res,
                 ...,
                 .body = NULL,
                 .type = 'application/json') {
  params <- list(...)
  .body <- .body %||% list(.body)

  req_args <-
    vctrs::vec_recycle_common(
      res = res,
      !!!params,
      .body = .body,
      .type = .type
    )
  reqs <- purrr::pmap(.l = req_args, .f = req)

  reqs
}
