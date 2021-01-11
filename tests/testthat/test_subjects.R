library(testthat)

testthat::test_that('query for subjects data works (default args)', {
  s <- get_geco_subjects(TEST_PROJECT)
  testthat::expect_is(s, 'data.frame')
})

testthat::test_that('query for subjects data works when providing projectversion id', {
  pv <- get_latest_version(TEST_PROJECT)
  s <- get_geco_subjects(project_version_id = pv$id)
  testthat::expect_is(s, 'data.frame')
})
