
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


#' Fetch quantiles from the Generable API for a run
#'
#' Fetch quantiles of the specified parameter or predictive quantity from the Generable API
#' for a model run
#'
#' This function retrieves the quantiles from the Generable API for a model run. The user specifies
#' the parameter or predictive quantity to query. A data.frame in long format is returned with
#' `quantile`, `.variable`, and `.value` columns.
#'
#' Note: the quantile probabilites are set to 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, and 0.95. Other quantile
#' probabilities can be computed directly from the draws, if necessary.
#'
#' The parameters or predictive quantities for a particular model run can be found by calling
#' \code{\link{fetch_inference_runs}}. The `run_quantiles` column in the returned `data.frame` contains
#' two named lists describing the `parameter_names`or the `predictive_names`. When a predictive quantity
#' is requested, i.e. the name is in the `predictive_names` list, the user must set the `predictive`
#' argument in this function to `TRUE`.
#' 
#' Posterior quantiles are returned by default. The prior quantiles can be accessed by setting
#' the `type` argument to `prior`.
#'
#' This function retrieves biomarker data from the Generable API.
#' It requires authentication (see \code{\link{login}}) prior to use
#' and this pulls data from the Generable API.
#'
#' A project can be specified by using the project name or a specific project version.
#' If a project is specified using the name, data is fetched for the latest version of the project.
#' If a project is specified using the project version, the project name is ignored if it
#' is also included as an argument.
#'
#' @param parameter Name of the parameter or predictive quantity; this function does not take a vector.
#' @param run_id Run id; required.
#' @param project Project name
#' @param project_version_id Project version. If this is specified, the `project` argument is ignored.
#' @param predictive Set to `TRUE` if the parameter is a predictive quantity. Default is `FALSE`.
#' @param type Type of quantile to return, either posterior or prior. Default is `posterior`. To access
#'             prior quantiles, set this to `prior`.
#' @return `data.frame` of quantiles in long format with `quantile`, `.variable`, and `.value` columns
#'         for the 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, and 0.95 quantile probabilities.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
fetch_inference_quantiles <- function(parameter, run_id = NULL, project = NULL, project_version_id = NULL, predictive = F, type = c('posterior', 'prior')) {
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

