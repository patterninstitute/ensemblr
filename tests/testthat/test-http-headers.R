test_that("req_headers creates correct structure", {
  headers <- req_headers(accept = "application/json", content_type = "text/plain")

  expect_s3_class(headers, "ensemblr_req_hdr")
  expect_setequal(names(headers), req_header_names())
  expect_named(headers[!sapply(headers, is.null)], c("Accept", "Content-Type"))
  expect_equal(headers$Accept, "application/json")
  expect_equal(headers$`Content-Type`, "text/plain")
})

test_that("req_header_names returns the correct header names", {
  expected_headers <- c("Accept", "Accept-Encoding", "Content-Type", "Origin")
  expect_equal(req_header_names(), expected_headers)
})

test_that("res_header_names returns the correct response header names", {
  expected_headers <- c(
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
  expect_equal(res_header_names(), expected_headers)
})

test_that("req_headers creates a list with correct default values", {
  headers <- req_headers()
  expect_equal(headers$`Content-Type`, "application/json")
  expect_null(headers$Accept)
  expect_null(headers$`Accept-Encoding`)
  expect_null(headers$Origin)
  expect_s3_class(headers, "ensemblr_req_hdr")
})

test_that("req_headers assigns custom values correctly", {
  headers <- req_headers(accept = "application/xml", origin = "https://example.com")
  expect_equal(headers$Accept, "application/xml")
  expect_equal(headers$Origin, "https://example.com")
  expect_equal(headers$`Content-Type`, "application/json")
})

test_that("res_headers creates a list with correct default values", {
  headers <- res_headers()
  expect_equal(headers$`Access-Control-Allow-Origin`, "*")
  expect_equal(headers$`Content-Type`, "application/json")
  expect_null(headers$`Content-Length`)
  expect_s3_class(headers, "ensemblr_res_hdr")
})

test_that("res_headers assigns custom values correctly", {
  headers <- res_headers(
    content_length = "123",
    x_runtime = "0.123",
    x_rate_limit_limit = "100"
  )
  expect_equal(headers$`Content-Length`, "123")
  expect_equal(headers$`X-Runtime`, "0.123")
  expect_equal(headers$`X-RateLimit-Limit`, "100")
})

