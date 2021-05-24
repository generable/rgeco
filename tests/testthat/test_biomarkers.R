library(testthat)

test_login()
testthat::test_that('query for biomarkers data works (default args)', {
  d <- fetch_biomarkers(TEST_PROJECT)
  testthat::expect_is(d, 'data.frame')
  expect_no_duplicates(d, by = dplyr::vars(measurement_id))
})

testthat::test_that('query for biomarkers data works when providing projectversion id', {
  pv <- get_latest_version(TEST_PROJECT)
  d <- fetch_biomarkers(project_version_id = pv$id)
  testthat::expect_is(d, 'data.frame')
  expect_no_duplicates(d, by = dplyr::vars(measurement_id))
})

testthat::test_that('query for biomarkers data works when providing measurement_name', {
  d <- fetch_biomarkers(TEST_PROJECT, measurement_name = 'sld')
  testthat::expect_is(d, 'data.frame')
  expect_no_duplicates(d, by = dplyr::vars(measurement_id))
})

testthat::test_that('measurement_names endpoint returns list of values', {
  d <- list_biomarker_names(TEST_PROJECT)
  testthat::expect_is(d, 'character')
  testthat::expect_true(length(d) > 0)
})

