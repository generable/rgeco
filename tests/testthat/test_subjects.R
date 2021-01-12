library(testthat)

testthat::test_that('query for subjects data works (default args)', {
  test_login()
  s <- get_geco_subjects(TEST_PROJECT)
  testthat::expect_is(s, 'data.frame')
})

testthat::test_that('query for subjects data works when providing projectversion id', {
  test_login()
  pv <- get_latest_version_id(TEST_PROJECT)
  s <- get_geco_subjects(project_version_id = pv)
  testthat::expect_is(s, 'data.frame')
})
