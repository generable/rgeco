library(testthat)

test_login()
testthat::test_that('list_models does not error', {
  testthat::skip('No test user')
  m <- list_models(TEST_PROJECT)
  testthat::expect_is(m, 'data.frame')
})

testthat::test_that('find_runs does not error', {
  testthat::skip('No test user')
  m <- find_runs(TEST_PROJECT)
  testthat::expect_is(m, 'data.frame')
})

testthat::test_that('xarray can be imported', {
  testthat::skip('No test user')
  xr <- reticulate::import('xarray', convert = FALSE)
  testthat::expect_is(xr, class = 'python.builtin.module')
})
