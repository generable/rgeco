#' @importFrom magrittr %>%
.get_geco_trials_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  trials <- geco_api(TRIALS, project_version_id = pv_id)
  t <- as_dataframe.geco_api_data(trials, flatten_names = c('params')) %>%
    dplyr::rename_at(.vars = dplyr::vars(dplyr::matches(c('created_at', 'params', 'name', 'phase', 'internal_id', 'id'))),
                     .funs = ~ stringr::str_c('trial_', .x))
}

