#We can expect this R file to provide the necessary helper functios
#The reference to which headers were taken:
#https://github.com/Ensembl/ensembl-rest/wiki/HTTP-Headers

request_headers <-
  function(accept = NULL,
           accept_encoding = NULL,
           content_type =  "application/json",
           origin = NULL) {
    .headers <- list(
      Accept = accept,
      `Accept-Encoding` = accept_encoding,
      `Content-Type` = content_type,
      Origin = origin
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

    ..headers <-
      ..headers[!sapply(..headers, is.null)]

    structure(..headers, class = "response_headers")
  }
