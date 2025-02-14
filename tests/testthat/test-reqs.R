# Tests that require a connection to Ensembl's REST API.
skip_on_cran()
skip_if_offline()

##test for only reqs() function
test_that("reqs function creates correct number of requests", {
  res <- c("/endpoint1", "/endpoint2")
  param1 <- c("value1", "value2") # for optional parameters
  param2 <- c("a", "b")

  result <- reqs(res, param1 = param1, param2 = param2)

  expect_length(result, 2)
  expect_s3_class(result[[1]], "httr2_request") #check the object type
  expect_s3_class(result[[2]], "httr2_request")
})


#test_that("reqs function correctly handles parameter recycling", {
#  res <- c("/endpoint1", "/endpoint2", "/endpoint3")
#  param1 <- c("value1", "value2") # for optional parameters
#
#  result <- reqs(res, param1 = param1)
#
#  expect_length(result, 3)
#  expect_equal(httr2::req_url_query(result[[1]])$param1, "value1")
#  expect_equal(httr2::req_url_query(result[[2]])$param1, "value2")
#  expect_equal(httr2::req_url_query(result[[3]])$param1, "value1")
#})

test_that("reqs() correctly applies custom headers", {
  res <- "/endpoint"
  custom_headers <- req_headers(accept = "text/x-fasta")

  result <- reqs(res, .headers = custom_headers)

  expect_length(result, 1)
  expect_equal(httr2::req_headers(result[[1]])$headers$Accept, "text/x-fasta")
})

#test_that("reqs() correctly interpolates variables in resource path", {
#  res <- "/endpoint/{var1}/{var2}"
#  var1 <- c("a", "b")
#  var2 <- c("x", "y")
#
#  result <- reqs(res, var1 = var1, var2 = var2)
#
#  expect_length(result, 2)
#  expect_match(httr2::req_url_path(result[[1]]), "/endpoint/a/x")
#  expect_match(httr2::req_url_path(result[[2]]), "/endpoint/b/y")
#})

## more tests for `reqs()` function

test_that("reqs() correctly vectorizes parameters", {
  res_path <- "/example/resource/{param1}/{param2}"
  req_list <- reqs(
    res = res_path,
    param1 = "value1",
    param2 = c("valA", "valB"),
    .headers = req_headers(content_type = "application/json")
  )

  expect_equal(length(req_list), 2)

  expect_equal(req_list[[1]]$url,
               "https://rest.ensembl.org/example/resource/value1/valA")
  expect_equal(req_list[[2]]$url,
               "https://rest.ensembl.org/example/resource/value1/valB")

  req_list2 <- reqs(
    res = res_path,
    param1 = c("val1", "val2"),
    param2 = c("A", "B"),
    .headers = req_headers(content_type = "application/json")
  )

  expect_equal(length(req_list2), 2)

  expect_equal(req_list2[[1]]$url,
               "https://rest.ensembl.org/example/resource/val1/A")
  expect_equal(req_list2[[2]]$url,
               "https://rest.ensembl.org/example/resource/val2/B")

  # vectorized parameters with different lengths (should throw an error)
  expect_error(reqs(
    res = res_path,
    param1 = c("val1", "val2", "val3"),
    param2 = c("A", "B"),
    .headers = req_headers(content_type = "application/json")
  ), "Can't recycle `res` \\(size 3\\) to match `param2` \\(size 2\\).")

  # no parameters passed (edge case)
  req_list3 <- reqs(
    res = "/example/resource",
    .headers = req_headers(content_type = "application/json")
  )

  expect_equal(length(req_list3), 1)
  expect_equal(req_list3[[1]]$url, "https://rest.ensembl.org/example/resource")
}
)
