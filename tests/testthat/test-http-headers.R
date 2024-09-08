test_that("req_headers creates correct structure", {
  headers <- req_headers(accept = "application/json", content_type = "text/plain")

  # Check object class
  expect_s3_class(headers, "ensemblr_req_hdr")

  # Check that list names match the expect header names.
  expect_setequal(names(headers), req_header_names())

  # Check that only only defined headers are different from `NULL`.
  expect_named(headers[!sapply(headers, is.null)], c("Accept", "Content-Type"))

  # Check that defined headers have the assigned values.
  expect_equal(headers$Accept, "application/json")
  expect_equal(headers$`Content-Type`, "text/plain")
})

# test_that("res_headers creates correct structure", {
#   headers <- response_headers(content_type = "application/json", x_runtime = "0.01")
#
#   # Check that list names match the expect header names.
#   expect_setequal(names(headers), res_header_names())
#
#   # Check that only only defined headers are different from `NULL`.
#   expect_named(headers[!sapply(headers, is.null)], c("Accept", "Content-Type"))
#
#   # Check that defined headers have the assigned values.
#   expect_equal(headers$Accept, "application/json")
#   expect_equal(headers$`Content-Type`, "text/plain")
#
#
#   expect_s3_class(headers, "ensemblr_res_hdr")
#   expect_named(headers, c("access_control_allow_origin", "content_type", "x_runtime"))
#   expect_equal(headers$content_type, "application/json")
#   expect_equal(headers$x_runtime, "0.01")
# })
