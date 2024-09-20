# Tests that require a connection to Ensembl's REST API.
skip_on_cran()
skip_if_offline()

## tests for `req()` function

test_that("`req()` builds correct request", {
  res <- "/cafe/genetree/member/symbol/{species}/{symbol}"
  test_req <- req(
    res,
    species = "homo_sapiens",
    symbol = "BRCA2",
    version = NULL,
    .headers = req_headers(accept = "application/json")
  )

  expected_endpoint <- "/cafe/genetree/member/symbol/homo_sapiens/BRCA2"
  expect_identical(test_req$url, paste0(base_url(), expected_endpoint))
  expect_identical(test_req$headers$Accept, "application/json")
  expect_identical(test_req$headers$`Accept-Encoding`, NULL)
  expect_identical(test_req$headers$`Content-Type`, "application/json")
  expect_identical(test_req$headers$Origin, NULL)
  expect_identical(test_req$options$useragent, user_agent())
})

test_that("`req()` function raises error for missing required parameters", {

  res <- "/cafe/genetree/member/symbol/{species}/{symbol}"
  expect_error(req(res, species = "homo_sapiens"), "object 'symbol' not found")
  expect_error(req(res, symbol = "BRCA2"), "object 'species' not found")
  expect_no_error(req(res, species = "homo_sapiens", symbol = "BRCA2"))

})

test_that("`req()` sets request body, if provided", {
  res <- "/cafe/genetree/member/symbol/{species}/{symbol}"
  test_req <- req(
    res,
    species = "homo_sapiens",
    symbol = "BRCA2",
    .body = "test body content",
    .headers = req_headers(content_type = "text/plain")
  )

  expect_identical(test_req$body$data, "test body content")
  expect_identical(test_req$headers$`Content-Type`, "text/plain")
})

# tests for optional parameters only
test_that("`req()` function handles optional parameters correctly", {
  test_req <- req("/path/to/resource",
                  optional_param1 = "value1",
                  optional_param2 = "value2")
  expect_identical(
    test_req$url,
    paste0(
      base_url(),
      "/path/to/resource",
      "?optional_param1=",
      "value1",
      "&optional_param2=",
      "value2"
    )
  )
})

test_that("`req()` function does not set body when `.body` is NULL", {
  test_req <- req(
    "/cafe/genetree/member/symbol/{species}/{symbol}",
    species = "homo_sapiens",
    symbol = "BRCA2",
    .headers = req_headers(content_type = "application/json")
  )
  expect_null(test_req$body)
})

test_that("req function handles invalid or null headers", {
  test_req <- req("/path/to/resource", .headers = req_headers())
  expect_false(any(is.null(test_req$headers)))
})

test_that("req function correctly handles special characters in parameters", {
  test_req <- req("/path", query_param = "special?chars&")
  expected_url <- "https://rest.ensembl.org/path?query_param=special%3Fchars%26"
  expect_identical(test_req$url, expected_url)
})

# in case of missing http headers argument (using defaults);
# content type defaults to application/json when not provided
test_that("req function handles default headers", {
  test_req <- req("/path/to/resource")
  expect_identical(test_req$headers$`Content-Type`, "application/json")
  expect_null(test_req$headers$Accept)
  expect_null(test_req$headers$Origin)
})

# ------------- GET Request Tests -------------

test_that("GET req sends a valid GET request with valid id", {
  res <- req("/archive/id/{id}", id = "ENSG00000157764") |>
    httr2::req_perform()

  expect_s3_class(res, "httr2_response")
  expect_identical(res$url, "https://rest.ensembl.org/archive/id/ENSG00000157764")
  expect_identical(res$method, "GET")
  expect_true("Content-Type" %in% names(res$headers))
  expect_identical(res$headers$`Content-Type`, "application/json")
})

test_that("GET req sends correct callback parameter in URL", {
  res <- req("/archive/id/{id}", id = "ENSG00000157764", callback = "randomlygeneratedname")
  expected_url <- "https://rest.ensembl.org/archive/id/ENSG00000157764?callback=randomlygeneratedname"
  expect_identical(res$url, expected_url)
})

test_that("GET req sets correct headers", {
  res <- req("/archive/id/{id}", id = "ENSG00000157764")
  expect_identical(res$headers$`Content-Type`, "application/json")
})

# ------------- POST Request Tests -------------

## to be done
