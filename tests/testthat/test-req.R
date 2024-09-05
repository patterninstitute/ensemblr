# start with basic function
test_that("vars_in_braces extracts variables correctly", {
  expect_equal(vars_in_braces("Hello {world}"), "world")
  expect_equal(vars_in_braces("/{species}/{symbol}"), c("species", "symbol"))
  expect_equal(vars_in_braces("No braces here"), character(0))
  expect_equal(vars_in_braces("{a} {b} {c}"), c("a", "b", "c"))
})

testthat::skip_on_cran() # the next tests depends on ensembl database,
#hence these tests will be skipped for cran, to avoid
#that in the case the ensembl database is down the test have to go through

testthat::skip_if_offline() # if you are offline

test_that("req function builds correct request", {
  test_req <- req("/cafe/genetree/member/symbol/{species}/{symbol}",
                  species = "homo_sapiens",
                  symbol = "BRCA2",
                  version = NULL,
                  .headers = request_headers(accept = "application/json"))

  expect_equal(test_req$url, paste0(base_url(), "/cafe/genetree/member/symbol/homo_sapiens/BRCA2"))
  expect_equal(test_req$headers$Accept, "application/json")
  expect_equal(test_req$headers$`Accept-Encoding`, NULL)
  expect_equal(test_req$headers$`Content-Type`, "application/json")
  expect_equal(test_req$headers$Origin, NULL)
  expect_equal(test_req$options$useragent, user_agent())
})

#test that the function raises an error when required parameters are missing
test_that("req function raises error for missing required parameters", {
  expect_error(
    req("/cafe/genetree/member/symbol/{species}/{symbol}",
        species = "homo_sapiens"),
    "object 'symbol' not found"
  )
  expect_error(
    req("/cafe/genetree/member/symbol/{species}/{symbol}", symbol = "BRCA2"),
    "object 'species' not found"
  )
})

test_that("req function sets body if provided", {
  test_req <- req("/cafe/genetree/member/symbol/{species}/{symbol}",
                  species = "homo_sapiens",
                  symbol = "BRCA2",
                  .body = "test body content",
                  .headers = request_headers(content_type = "text/plain"))
  expect_equal(charToRaw(test_req$body$data), charToRaw("test body content"))
  expect_equal(test_req$headers$`Content-Type`, "text/plain")
})

# tests for optional parameters only
test_that("req function handles optional parameters correctly", {
  test_req <- req("/path/to/resource",
                  optional_param1 = "value1",
                  optional_param2 = "value2")
  expect_equal(test_req$query_params, list(optional_param1 = "value1",
                                           optional_param2 = "value2"))
  expect_equal(test_req$url, paste0(base_url(), "/path/to/resource",
                                    "?optional_param1=", "value1",
                                    "&optional_param2=", "value2"))
})

test_that("req function does not set body when .body is NULL", {
  test_req <- req("/cafe/genetree/member/symbol/{species}/{symbol}",
                  species = "homo_sapiens",
                  symbol = "BRCA2",
                  .headers = request_headers(content_type = "application/json"))
  expect_null(test_req$body)
})

# test for invalid or `NULL` header inputs
test_that("req function handles invalid headers", {
  test_req <- req("/path/to/resource", .headers = request_headers())
  expect_false(any(is.null(test_req$headers)))
})

# Test for edge cases with special characters in parameters
test_that("req function correctly handles special characters in parameters", {
  test_req <- req("/path", query_param = "special?chars&")
  expect_equal(test_req$query_params, list(query_param = "special?chars&"))
})

# in case of missing http headers argument (using defaults);
# content type defaults to application/json when not provided
test_that("req function handles default headers", {
  test_req <- req("/path/to/resource")
  expect_equal(test_req$headers$`Content-Type`, "application/json")
  expect_equal(test_req$headers$Accept, NULL)
  expect_equal(test_req$headers$Origin, NULL)
})
