# start with basic function
test_that("vars_in_braces extracts variables correctly", {
  expect_equal(vars_in_braces("Hello {world}"), "world")
  expect_equal(vars_in_braces("/{species}/{symbol}"), c("species", "symbol"))
  expect_equal(vars_in_braces("No braces here"), character(0))
  expect_equal(vars_in_braces("{a} {b} {c}"), c("a", "b", "c"))
})
