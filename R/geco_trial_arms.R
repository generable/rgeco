.get_geco_trial_arms_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  trial_arms <- geco_api(TRIALARMS, project_version_id = pv_id)
  ta <- as_dataframe.geco_api_data(trial_arms, flatten_names = c('params', 'regimen')) %>%
    dplyr::rename_at(.vars = dplyr::vars(created_at, params, id), .funs = ~ stringr::str_c('trial_arm_', .x))
}

