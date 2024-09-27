test_that("base_url() returns correct URL", {
  ensembl_url <- "https://rest.ensembl.org"
  expect_equal(base_url(), ensembl_url)
})
