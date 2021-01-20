library(testthat)

test_login()
testthat::test_that('query for subjects data works (default args)', {
  s <- get_geco_subjects(TEST_PROJECT)
  testthat::expect_is(s, 'data.frame')
  expect_no_duplicates(s, by = dplyr::vars(subject_id))
})

testthat::test_that('query for subjects data works when providing projectversion id', {
  pv <- get_latest_version_id(TEST_PROJECT)
  s <- get_geco_subjects(project_version_id = pv)
  testthat::expect_is(s, 'data.frame')
  expect_no_duplicates(s, by = dplyr::vars(subject_id))
})

# test functions used within subjects data, to isolate any errors

testthat::test_that('regimens data are unique per regimen_id', {
  reg <- rgeco::.get_geco_regimens_data(TEST_PROJECT)
  expect_no_duplicates(reg, by = dplyr::vars(regimen_id))
})

testthat::test_that('trials data are unique per trial_id', {
  d <- rgeco::.get_geco_trials_data(TEST_PROJECT)
  expect_no_duplicates(d, by = dplyr::vars(trial_id))
})

testthat::test_that('trial_arms data are unique per trial_arm_id', {
  d <- rgeco::.get_geco_trial_arms_data(TEST_PROJECT)
  expect_no_duplicates(d, by = dplyr::vars(trial_arm_id))
})


testthat::test_that('events data are unique per event_id', {
  d <- rgeco::.get_geco_events_data(TEST_PROJECT)
  expect_no_duplicates(d, by = dplyr::vars(event_id))
})
