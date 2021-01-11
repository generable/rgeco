library(testthat)

testthat::test_that('query for biomarkers data works (default args)', {
  test_login()
  d <- get_geco_biomarkers(TEST_PROJECT)
  testthat::expect_is(d, 'data.frame')
})

testthat::test_that('query for biomarkers data works when providing projectversion id', {
  test_login()
  pv <- get_latest_version(TEST_PROJECT)
  d <- get_geco_biomarkers(project_version_id = pv$id)
  testthat::expect_is(d, 'data.frame')
})

testthat::test_that('query for biomarkers data works when providing measurement_name', {
  test_login()
  d <- get_geco_biomarkers(TEST_PROJECT, measurement_name = 'sld')
  testthat::expect_is(d, 'data.frame')
})


