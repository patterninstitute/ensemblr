test_that("user_agent() works", {
  user_agent_desc <- "ensemblr (https://www.pattern.institute/ensemblr)"
  expect_equal(user_agent(), user_agent_desc)
})
