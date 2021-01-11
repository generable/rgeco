library(testthat)

testthat::test_that('query for dose data works (default args)', {
  testthat::expect_warning({
    d <- get_geco_doses(TEST_PROJECT)
  })
  testthat::expect_is(d, 'data.frame')
})
