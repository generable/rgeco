
#' Fetch draws of parameter or predictive quantities for a model run
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
fetch_inference_draws <- function(parameter, project = NULL, project_version_id = NULL, run_id = NULL, predictive = F, type = c('posterior', 'prior')) {
  type <- match.arg(type, several.ok = F)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (is.null(run_id)) {
    run_id = .get_default_run(parameter = parameter, project_version_id = pv_id, predictive = predictive, type = type)
    if (length(run_id) == 0) {
      # TODO list valid values
      stop(glue::glue('No runs for this project version ({pv_id}) found with the requested parameter ({parameter}).'))
    }
  }
  if (isTRUE(predictive)) {
    draws <- geco_api(IPDRAWS, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  } else {
    draws <- geco_api(IDRAWS, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  }
  d <- convert_draws_to_df(draws, name = parameter)
}

#' Fetch quantiles of parameter or predictive quantity draws for a model run
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
fetch_inference_quantiles <- function(parameter, project = NULL, project_version_id = NULL, run_id = NULL, predictive = F, type = c('posterior', 'prior')) {
  type <- match.arg(type, several.ok = F)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (is.null(run_id)) {
    run_id = .get_default_run(parameter = parameter, project_version_id = pv_id, predictive = predictive, type = type)
    if (length(run_id) == 0) {
      # TODO list valid values
      stop(glue::glue('No runs for this project version ({pv_id}) found with the requested parameter ({parameter}).'))
    }
  }
  if (isTRUE(predictive)) {
    quantiles <- geco_api(IPTILES, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  } else {
    quantiles <- geco_api(ITILES, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  }
  q <- convert_xarray_to_df(quantiles, name = parameter)
}

