library(testthat)

testthat::test_that('query for dose data works (default args)', {
  d <- testthat::expect_warning(
    get_geco_doses(TEST_PROJECT)
  )
  testthat::expect_is(d, 'data.frame')
})
