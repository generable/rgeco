
library(testthat)

test_login()

testthat::test_that('generic biomarkers data can be passed through to annotate_pkpd_data', {
  d <- get_geco_biomarkers(TEST_PROJECT, measurement_name = 'sld')
  testthat::expect_is(d, 'data.frame')
  expect_no_duplicates(d, by = dplyr::vars(measurement_id))
  annotated <- annotate_pkpd_data(d, pk_measure = 'sld')
  testthat::expect_is(annotated, 'data.frame')
  expect_no_duplicates(annotated, by = dplyr::vars(measurement_id))
})

