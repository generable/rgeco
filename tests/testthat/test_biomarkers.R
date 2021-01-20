library(testthat)

test_login()
testthat::test_that('query for biomarkers data works (default args)', {
  d <- get_geco_biomarkers(TEST_PROJECT)
  testthat::expect_is(d, 'data.frame')
  expect_no_duplicates(d, by = dplyr::vars(measurement_id))
})

testthat::test_that('query for biomarkers data works when providing projectversion id', {
  pv <- get_latest_version(TEST_PROJECT)
  d <- get_geco_biomarkers(project_version_id = pv$id)
  testthat::expect_is(d, 'data.frame')
  expect_no_duplicates(d, by = dplyr::vars(measurement_id))
})

testthat::test_that('query for biomarkers data works when providing measurement_name', {
  d <- get_geco_biomarkers(TEST_PROJECT, measurement_name = 'sld')
  testthat::expect_is(d, 'data.frame')
  expect_no_duplicates(d, by = dplyr::vars(measurement_id))
})


