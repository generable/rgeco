library(testthat)

testthat::test_that('login prints useful error message for incorrect password', {
  testthat::expect_error(login(user = 'notauser', password = 'wrongpassword'), regexp = 'password')
})

testthat::test_that('project_versions endpoint works', {
  test_login()
  pv <- geco_api(PROJECTVERSIONS, project = TEST_PROJECT)
  pv2 <- get_latest_version(project = TEST_PROJECT)
  testthat::expect_is(pv2$id, 'character')
})
