#' @importFrom magrittr %>%
.fetch_trial_arms_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  trial_arms <- geco_api(TRIALARMS, project_version_id = pv_id)
  if (httr::http_error(trial_arms$response)) {
    ta <- as_dataframe.geco_api_data(trial_arms, flatten_names = c('params', 'regimen'))
  } else {
    trial_arms_content <- trial_arms$content %>%
      purrr::map(purrr::list_modify, cohorts = NULL)
    ta <- as_dataframe.geco_api_data(content = trial_arms_content, flatten_names = c('params', 'regimen'))
  }
  if (nrow(ta) > 0) {
    if ('id' %in% names(ta)) {
      ta <- ta %>%
        dplyr::rename(trial_arm_id = .data$id)
    }
    if ('regimen' %in% names(ta)) {
      ta <- ta %>%
        tidyr::hoist(.data$regimen, 'id') %>%
        tidyr::unnest_longer(.data$id) %>%
        dplyr::rename(trial_arm_regimen_id = .data$id) %>%
        dplyr::distinct()
      regimens <- .fetch_regimens_data(project_version_id = pv_id) %>%
        dplyr::rename_all(.add_prefix, 'trial_arm')
      ta <- ta %>%
        dplyr::left_join(regimens, by = c('trial_arm_regimen_id'))
    }
    if ('params' %in% names(ta) && all(purrr::map_int(ta$params, nrow) == 1)) {
      ta <- ta %>%
        tidyr::unnest_wider(.data$params)
    }
  }
  suppressWarnings({
    ta <- ta %>%
      dplyr::rename_at(.vars = dplyr::vars(-dplyr::starts_with('trial_arm_'), -.data$trial_id),
                       .funs = ~ stringr::str_c('trial_arm_', .x))
  })
  ta
}

