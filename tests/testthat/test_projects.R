library(testthat)

testthat::test_that('query for projects data works (default args)', {
  test_login()
  d <- fetch_projects()
  testthat::expect_is(d, 'data.frame')
})

testthat::test_that('query for projectversions data works (default args)', {
  test_login()
  pv <- fetch_projectversions(project = TEST_PROJECT)
  testthat::expect_is(pv, 'data.frame')
})
