
#' Fetch draws from the Generable API for a run
#'
#' Fetch draws of the specified parameter or predictive quantity from the Generable API
#' for a model run
#'
#' This function retrieves the draws from the Generable API for a model run. The user specifies
#' the parameter or predictive quantity to query. A data.frame in long format is returned with
#' `.chain`, `.iteration`, `.variable`, `.value`, and `.draw`.
#'
#' Note: this function may take a long time to return depending on the size of the parameter.
#' If a summary of the parameter is sufficient, use \code{\link{fetch_quantiles}} to
#' access the quantiles of the draw.
#'
#' The parameters or predictive quantities for a particular model run can be found by calling
#' \code{\link{list_parameter_names}} or \code{\link{list_predictive_names}}.
#'
#' Posterior draws are returned by default. The prior draws can be accessed by setting
#' the `type` argument to `prior`.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the draws from the Generable API.
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
#' @param type Type of quantile to return, either posterior or prior. Default is `posterior`. To access
#'             prior quantiles, set this to `prior`.
#' @return `data.frame` of draws in long format with `.chain`, `.iteration`, `.variable`, `.value`, and
#'         `.draw`.
#'
#' @seealso \code{\link{fetch_quantiles}}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
fetch_draws <- function(parameter, run_id, project = NULL, project_version_id = NULL, type = c('posterior', 'prior')) {
  type <- match.arg(type, several.ok = F)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (is.null(run_id)) {
    stop(glue::glue('Please specify the run_id'))
    #run_id = .get_default_run(parameter = parameter, project_version_id = pv_id, predictive = predictive, type = type)
    #if (length(run_id) == 0) {
    #  # TODO list valid values
    #  stop(glue::glue('No runs for this project version ({pv_id}) found with the requested parameter ({parameter}).'))
    #}
  }
  if (length(parameter) != 1) {
    stop('This function takes one parameter at a time.')
  }

  parameter_names = list_parameter_names(run_id = run_id, project_version_id = pv_id)
  if (parameter %in% parameter_names) {
    draws <- geco_api(IDRAWS, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  } else {
    draws <- geco_api(IPDRAWS, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
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
#' \code{\link{list_parameter_names}} or \code{\link{list_predictive_names}}.
#'
#' Posterior quantiles are returned by default. The prior quantiles can be accessed by setting
#' the `type` argument to `prior`.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' A project can be specified by using the project name or a specific project version.
#' If a project is specified using the name, data is fetched for the latest version of the project.
#' If a project is specified using the project version, the project name is ignored if it
#' is also included as an argument.
#'
#' @param parameter Name of the parameter or predictive quantity; this function does not take a vector.
#'                  See \code{\link{list_parameter_names}} and \code{\link{list_predictive_names}}.
#' @param run_id Run id; required.
#' @param project Project name
#' @param project_version_id Project version. If this is specified, the `project` argument is ignored.
#' @param type Type of quantile to return, either posterior or prior. Default is `posterior`. To access
#'             prior quantiles, set this to `prior`.
#' @return `data.frame` of quantiles in long format with `quantile`, `.variable`, and `.value` columns
#'         for the 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, and 0.95 quantile probabilities.
#'
#' @seealso \code{\link{list_parameter_names}}, \code{\link{list_predictive_names}}, \code{\link{fetch_draws}}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
fetch_quantiles <- function(parameter, run_id, project = NULL, project_version_id = NULL, type = c('posterior', 'prior')) {
  type <- match.arg(type, several.ok = F)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (is.null(run_id)) {
    stop(glue::glue('Please specify the run_id'))
    #run_id = .get_default_run(parameter = parameter, project_version_id = pv_id, predictive = predictive, type = type)
    #if (length(run_id) == 0) {
    #  # TODO list valid values
    #  stop(glue::glue('No runs for this project version ({pv_id}) found with the requested parameter ({parameter}).'))
    #}
  }
  if (length(parameter) != 1) {
    stop('This function takes one parameter at a time.')
  }

  parameter_names = list_parameter_names(run_id = run_id, project_version_id = pv_id)
  if (parameter %in% parameter_names) {
    quantiles <- geco_api(ITILES, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  } else {
    quantiles <- geco_api(IPTILES, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  }
  q <- convert_xarray_to_df(quantiles, name = parameter) %>% arrange(.data$quantile)
}

