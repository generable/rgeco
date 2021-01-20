#' @importFrom magrittr %>%
.get_geco_trial_arms_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  trial_arms <- geco_api(TRIALARMS, project_version_id = pv_id)
  ta <- as_dataframe.geco_api_data(trial_arms, flatten_names = c('params', 'regimen')) %>%
    dplyr::mutate(trial_arm_regimen_id = regimen$id) %>%
    dplyr::select(-regimen) %>%
    dplyr::distinct()
  regimens <- .get_geco_regimens_data(project_version_id = pv_id) %>%
    dplyr::rename_all(.add_prefix, 'trial_arm')
  ta <- ta %>%
    dplyr::left_join(regimens, by = c('trial_arm_regimen_id'))
  suppressWarnings({
    ta <- ta %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'params', 'name', 'id'))),
                       .funs = ~ stringr::str_c('trial_arm_', .x))
  })
  ta
}

