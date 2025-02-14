req_header_names <- function() {
  c("Accept", "Accept-Encoding", "Content-Type", "Origin")
}

res_header_names <- function() {
  c(
    "Access-Control-Allow-Origin",
    "Content-Length",
    "Content-Type",
    "Retry-After",
    "X-Runtime",
    "X-RateLimit-Limit",
    "X-RateLimit-Reset",
    "X-RateLimit-Period",
    "X-RateLimit-Remaining"
  )
}

req_headers <-
  function(accept = NULL,
           accept_encoding = NULL,
           content_type =  "application/json", # as default
           origin = NULL) {

    headers_lst <-
      list(
        Accept = accept,
        `Accept-Encoding` = accept_encoding,
        `Content-Type` = content_type,
        Origin = origin
      )

    structure(headers_lst, class = "ensemblr_req_hdr")
  }

res_headers <-
  function(access_control_allow_origin = "*",
           content_length = NULL,
           content_type = "application/json",
           retry_after = NULL,
           x_runtime = NULL,
           x_rate_limit_limit = NULL,
           x_rate_limit_reset = NULL,
           x_rate_limit_period = NULL,
           x_rate_limit_remaining = NULL) {

    headers_lst <- list(
      `Access-Control-Allow-Origin` = access_control_allow_origin,
      `Content-Length` = content_length,
      `Content-Type` = content_type,
      `Retry-After` = retry_after,
      `X-Runtime` = x_runtime,
      `X-RateLimit-Limit` = x_rate_limit_limit,
      `X-RateLimit-Reset` = x_rate_limit_reset,
      `X-RateLimit-Period` = x_rate_limit_period,
      `X-RateLimit-Remaining` = x_rate_limit_remaining
    )

    structure(headers_lst, class = "ensemblr_res_hdr")
  }
