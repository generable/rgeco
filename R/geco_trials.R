.get_geco_trials_data <- function(project = NULL, project_version_id = NULL) {
  if (is.null(project_version_id))
    pv_id <- get_latest_version(project)$id
  else
    pv_id <- project_version_id
  trials <- geco_api(TRIALS, project_version_id = pv_id)
  t <- as_dataframe.geco_api_data(trials, flatten_names = c('params')) %>%
    dplyr::rename_at(.vars = dplyr::vars(created_at, id), .funs = ~ stringr::str_c('trial_', .x))
}

