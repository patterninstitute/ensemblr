base_url <- function() "https://rest.ensembl.org"

user_agent <- function() "ensemblr (https://www.pattern.institute/ensemblr)"

#' Create a new HTTP request
#'
#' [req()] creates an HTTP request object.
#'
#' @param res A resource (res) URL as a string. This string supports embedding
#'   of R variable names in curly braces whose values are looked up in parameter
#'   names supplied in `...` and interpolated.
#'
#' @param ... Name value pairs specifying query components or parameters.
#'
#' @param .body A literal string or raw vector to send as body.
#'
#' @param .headers An S3 list with class `ensemblr_req_hdr`. Use the helper
#' [req_headers()] to create such an object.
#'
#' @inherit httr2::request return
#'
#' @keywords internal
req <-
  function(res,
           ...,
           .body = NULL,
           .headers = req_headers()) {

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
      httr2::req_headers(!!!.headers) |>
      httr2::req_user_agent(user_agent())

    if (!is.null(.body)) {
      req <- httr2::req_body_raw(req, body = .body, type = .headers$content_type)
    }

    req
  }

reqs <- function(res,
                 ...,
                 .body = NULL,
                 .headers = req_headers()) {
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
