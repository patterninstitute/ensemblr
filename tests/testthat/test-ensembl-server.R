test_that("ensembl_server() works", {
  expect_equal(ensembl_server(), "https://rest.ensembl.org")
})
