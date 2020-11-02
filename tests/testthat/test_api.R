library(testthat)

testthat::test_that('project_versions endpoint works', {
  pv <- geco_api(PROJECTVERSIONS, project = 'houston')
  pv2 <- get_latest_version(project = 'houston')
  testthat::expect_is(pv2$id, 'character')
})

