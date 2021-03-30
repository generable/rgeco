
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
fetch_inference_draws <- function(project = NULL, project_version_id = NULL, run_id, parameter, predictive = F, type = c('posterior', 'prior')) {
  type <- match.arg(type, several.ok = F)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (isTRUE(predictive)) {
    draws <- geco_api(IPDRAWS, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  } else {
    draws <- geco_api(IDRAWS, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  }
  d <- convert_draws_to_df(draws, name = parameter)
}

#' @importFrom magrittr %>%
#' @importFrom rlang !!!
fetch_inference_quantiles <- function(project = NULL, project_version_id = NULL, run_id, parameter, predictive = F, type = c('posterior', 'prior')) {
  type <- match.arg(type, several.ok = F)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  summarized_parameter = stringr::str_c('summarized_', parameter)
  if (isTRUE(predictive)) {
    draws <- geco_api(IPDRAWS, project_version_id = pv_id, run_id=run_id, parameter=summarized_parameter, type=type)
  } else {
    draws <- geco_api(IDRAWS, project_version_id = pv_id, run_id=run_id, parameter=summarized_parameter, type=type)
  }
  d <- convert_xarray_to_df(draws, name = summarized_parameter)
}

