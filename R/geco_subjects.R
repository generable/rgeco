get_geco_subjects <- function(project, project_version_id = NULL) {
  if (is.null(project_version_id))
    pv_id <- get_latest_version(project)$id
  else
    pv_id <- project_version_id
  trials <- geco_api(TRIALS, project_version_id = pv_id)
  trial_arms <- geco_api(TRIALARMS, project_version_id = pv_id)
  subjects <- geco_api(SUBJECTS, project_version_id = pv_id)
  s <- as_dataframe.geco_api_data(subjects) %>%
    dplyr::left_join(as_dataframe.geco_api_data(trial_arms), by = ('trial_arm_id' = 'id')) %>%
    dplyr::left_join(as_dataframe.geco_api_data(trials), by = c('trial_id' = 'id'))
}

