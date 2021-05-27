
#' Fetch draws from the Generable API for a run
#'
#' Fetch draws of the specified parameter or predictive quantity from the Generable API
#' for a model run
#'
#' This function retrieves the draws from the Generable API for a model run. The user specifies
#' the parameter or predictive quantity to query. A data.frame in long format is returned with
#' `run_id`, `.chain`, `.iteration`, `.variable`, `.value`, and `.draw`, plus the coordinates (indices)
#' for the multi-dimensional parameters.
#'
#' For example, the dimensions for predicted_survival,
#' the predicted survival per subject over time will be `subject` and `survival_time`.
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
#' Both the parameter & run_id arguments are vectorized. In the case of multiple values,
#' the rows returned for each run and/or parameter are concatenated into a single data.frame. This is
#' intended for use where the runs use the same model, and parameters are similar dimension.
#'
#' @param parameter Name of the parameter or predictive quantity
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
  checkmate::assert_character(parameter, unique = TRUE)
  checkmate::assert_character(run_id, unique = TRUE)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)

  queries <- purrr::cross2(run_id, parameter)
  show_progress <- length(queries) > 5 && interactive()
  if (isTRUE(show_progress)) {
    pb <- dplyr::progress_estimated(length(queries))
    futile.logger::flog.info(glue::glue('Fetching draws for {length(queries)} run_idXparameter combinations.'))
  } else {
    pb <- NULL
  }
  queries %>%
    purrr::map_dfr(~ .fetch_draws_per_parameter_run(run_id = .x[[1]], parameter = .x[[2]],
                                                    project_version_id=pv_id,
                                                    type=type,
                                                    pb=pb))
}

.fetch_draws_per_parameter_run <- function(run_id, parameter, project_version_id, type, pb = NULL) {
  if (is.null(pb)) {
    futile.logger::flog.info(glue::glue('Fetching draws for {parameter} from run {run_id}.'))
  }
  parameter_names <- list_parameter_names(run_id = run_id, project_version_id = project_version_id)
  if (parameter %in% parameter_names) {
    draws <- geco_api(IDRAWS, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type)
  } else {
    draws <- geco_api(IPDRAWS, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type)
  }
  if (!is.null(pb)) {
    pb$tick()$print()
  }
  d <- convert_draws_to_df(draws, name = parameter) %>%
    dplyr::mutate(run_id = run_id)
}


#' Fetch quantiles from the Generable API for a run
#'
#' Fetch quantiles of the specified parameter or predictive quantity from the Generable API
#' for a model run
#'
#' This function retrieves the quantiles from the Generable API for a model run. The user specifies
#' the parameter or predictive quantity to query. A data.frame in long format is returned with
#' `quantile`, `.variable`, `run_id`, and `.value` columns, along with the dimensions over which the
#' parameter is estimated such as `subject`, `survival_time`, or `study`.
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
#' Both the parameter & run_id arguments are vectorized. In the case of multiple values,
#' the rows returned for each run and/or parameter are concatenated into a single data.frame. This is
#' intended for use where the runs use the same model, and parameters are similar dimension.
#'
#' @param parameter Name of the parameter or predictive quantity
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
#' @import checkmate
#' @export
fetch_quantiles <- function(parameter, run_id, project = NULL, project_version_id = NULL, type = c('posterior', 'prior')) {
  type <- match.arg(type, several.ok = F)
  checkmate::assert_character(parameter, unique = TRUE)
  checkmate::assert_character(run_id, unique = TRUE)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)

  queries <- purrr::cross2(run_id, parameter)
  show_progress <- length(queries) > 5 && interactive()
  if (isTRUE(show_progress)) {
    pb <- dplyr::progress_estimated(length(queries))
    futile.logger::flog.info(glue::glue('Querying {type} quantiles for {length(queries)} run_idXparameter combinations.'))
  } else {
    pb <- NULL
  }
  queries %>%
    purrr::map_dfr(~ .fetch_quantiles_per_parameter_run(run_id = .x[[1]], parameter = .x[[2]],
                                                    project_version_id=pv_id,
                                                    type=type,
                                                    pb=pb))
}

.fetch_quantiles_per_parameter_run <- function(run_id, parameter, project_version_id, type, pb = NULL) {
  if (is.null(pb)) {
    futile.logger::flog.info(glue::glue('Querying {type} quantiles of {parameter} from run {run_id}.'))
  }
  parameter_names = list_parameter_names(run_id = run_id, project_version_id = project_version_id)
  if (parameter %in% parameter_names) {
    quantiles <- geco_api(ITILES, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type)
  } else {
    quantiles <- geco_api(IPTILES, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type)
  }
  if (!is.null(pb)) {
    pb$tick()$print()
  }
  q <- convert_xarray_to_df(quantiles, name = parameter) %>%
    dplyr::mutate(run_id = run_id) %>%
    dplyr::arrange(.data$quantile)
}
