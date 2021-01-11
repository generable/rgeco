library(testthat)

testthat::test_that('project_versions endpoint works', {
  pv <- geco_api(PROJECTVERSIONS, project = TEST_PROJECT)
  pv2 <- get_latest_version(project = TEST_PROJECT)
  testthat::expect_is(pv2$id, 'character')
})

