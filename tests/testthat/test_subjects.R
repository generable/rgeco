library(testthat)

testthat::test_that('query for subjects data works (default args)', {
  s <- get_geco_subjects('houston')
  testthat::expect_is(s, 'data.frame')
})

testthat::test_that('query for subjects data works when providing projectversion id', {
  pv <- get_latest_version('houston')
  s <- get_geco_subjects('houston', project_version_id = pv$id)
  testthat::expect_is(s, 'data.frame')
})
