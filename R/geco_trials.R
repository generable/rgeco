#' @importFrom magrittr %>%
.fetch_trials_data <- function(project = NULL, project_version_id = NULL, where = c()) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  trials_filter <- .prepare_filter(where, endpoint = 'TRIALS')
  trials <- geco_api(TRIALS, project_version_id = pv_id, url_query_parameters = trials_filter)
  t <- as_dataframe.geco_api_data(trials, flatten_names = c('params'))
  suppressWarnings({
    t <- t %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'params', 'name', 'phase', 'internal_id', 'id'))),
                       .funs = ~ stringr::str_c('trial_', .x))
  })
  t %>%
    .apply_filters(where)
}
