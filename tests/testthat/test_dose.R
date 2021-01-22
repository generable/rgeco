library(testthat)

testthat::test_that('query for dose data works (default args)', {
  test_login()
  d <- testthat::expect_warning(
    fetch_doses(TEST_PROJECT)
  )
  testthat::expect_is(d, 'data.frame')
})
