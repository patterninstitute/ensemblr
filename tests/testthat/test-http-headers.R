test_that("request_headers creates correct structure", {
  headers <- request_headers(accept = "application/json", content_type = "text/plain")
  expect_s3_class(headers, "request_headers")
  expect_named(headers, c("accept", "content_type"))
  expect_equal(headers$accept, "application/json")
  expect_equal(headers$content_type, "text/plain")
  expect_null(headers$no_existing_param)
  # (if someone passes a number instead of a string, for instance)
  expect_false(is.character(request_headers(accept = 123)$accept), "is.character")
})

test_that("response_headers creates correct structure", {
  headers <- response_headers(content_type = "application/json", x_runtime = "0.01")
  expect_s3_class(headers, "response_headers")
  expect_named(headers, c("access_control_allow_origin", "content_type", "x_runtime"))
  expect_equal(headers$content_type, "application/json")
  expect_equal(headers$x_runtime, "0.01")
})
