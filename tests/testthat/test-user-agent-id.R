test_that("user_agent_id() works", {
  user_agent_desc <- "ensemblr (https://www.pattern.institute/ensemblr)"
  expect_equal(user_agent_id(), user_agent_desc)
})
