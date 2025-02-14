callback_warning <- function() {
  warning(
    "This happen when `callback` is NULL. We are working on it.
     if you are curious please checkout for more info:
     https://github.com/Ensembl/ensembl-rest/wiki/CORS-And-JSONP#json-p or
     https://httr2.r-lib.org/reference/req_perform_stream.html"
  )
}
