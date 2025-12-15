
#' Fetch draws from the Generable API for a run
#'
#' Fetch draws of the specified parameter or predictive quantity from the Generable API
#' for a model run
#'
#' This function retrieves the draws from the Generable API for one or more model runs.
#'
#' The user provides a parameter or predictive quantity to query.
#'
#' The returned object is a data.frame in long format, mimicking the structure of \code{\link[posterior:draws_df]{draws_df}} in the
#' \link[posterior:posterior-package]{posterior} package. The data.frame has columns `run_id`, `.chain`, `.iteration`, `.variable`, `.value`, and `.draw`, plus the coordinates (indices)
#' for the multi-dimensional parameters.
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
#' @note
#' A project can be specified by using the project name or a specific project version.
#' \enumerate{
#'   \item If a project is specified using the name, data is fetched for the latest version of the project.
#'   \item If a project is specified using the project version, the project name is not required.
#'   \item If neither a project nor a project version is provided, the default project or project version is used. These are set by the environment variables GECO_API_PROJECT and GECO_API_PROJECT_VERSION
#' }
#'
#' @note
#' Both the parameter & run_id arguments are vectorized. In the case of multiple values,
#' the rows returned for each run and/or parameter are concatenated into a single data.frame. This is
#' intended for use where the runs use the same model, and parameters are similar dimension.
#'
#' @param parameter (str) [required] Name(s) of the parameters or predictive quantities to be retrieved.
#'                  See \code{\link{list_parameter_names}} and \code{\link{list_predictive_names}}.
#' @param run_id (str) [required] Run id(s). See \code{\link{find_runs}} to see runs available for this project.
#' @param project (str) Project name. If NULL, defaults to value of environment variable GECO_API_PROJECT
#' @param project_version_id (str) Project version. If NULL, defaults to the most recent version of the project if provided, or the value of environment variable GECO_API_PROJECT_VERSION
#' @param type (str) Type of quantile to return, either posterior or prior. Default is `posterior`. To access
#'             prior quantiles, set this to `prior`.
#' @param quiet (bool) if TRUE, suppress informative messages
#' @param .dots (lists) advanced feature used to limit draws returned (alternate version of providing ...)
#' @param ... (lists) advanced feature used to limit draws returned.
#'     Example: draws = c(0:10), chains = 0 to limit to first chain, 10 draws. Filters are applied server-side so they use 0-indexing.
#'     Example: trial_arm = unique(subjects$trial_arm_id) to limit draws to a set of trial arms, for a parameter estimated per trial arm
#'     However, use of this feature requires that one be familiar with both the dimensions of a particular parameter and their names server-side.
#' @return `data.frame` of draws in long format with `.chain`, `.iteration`, `.variable`, `.value`, and
#'         `.draw`.
#'
#' @seealso \code{\link{fetch_quantiles}}, \code{\link{list_parameter_names}}, \code{\link{list_predictive_names}}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
fetch_draws <- function(parameter, run_id, project = NULL, project_version_id = NULL, type = c('posterior', 'prior'), quiet = FALSE,
                        .dots = list(), ...) {
  extra_dots <- rlang::list2(...)
  filters <- purrr::list_modify(.dots, !!!extra_dots)
  filters <- .check_format(filters, alert = TRUE)
  type <- match.arg(type, several.ok = F)
  checkmate::assert_character(parameter, unique = TRUE)
  checkmate::assert_character(run_id, unique = TRUE)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)

  queries <- purrr::cross2(run_id, parameter)
  show_progress <- length(queries) > 5 && interactive()
  if (isTRUE(show_progress)) {
    pb <- dplyr::progress_estimated(length(queries))
    if (isFALSE(quiet))
      futile.logger::flog.info(glue::glue('Fetching draws for {length(queries)} run_idXparameter combinations.'))
  } else {
    pb <- NULL
  }
  queries %>%
    purrr::map_dfr(~ .fetch_draws_per_parameter_run(run_id = .x[[1]], parameter = .x[[2]],
                                                    project_version_id=pv_id,
                                                    type=type,
                                                    pb=pb,
                                                    quiet=quiet,
                                                    where=filters
                                                    ))
}

.fetch_draws_per_parameter_run <- function(run_id, parameter, project_version_id, type, pb = NULL, quiet = FALSE, where = list(), split = TRUE) {
  if (is.null(pb) && interactive() && isFALSE(quiet)) {
    futile.logger::flog.info(glue::glue('Fetching draws for {parameter} from run {run_id}.'))
  }
  if (isTRUE(split)) {
    split_filter <- .split_filter(where)
    results <- split_filter %>%
      purrr::map_dfr(~ .fetch_draws_per_parameter_run(run_id = run_id, parameter = parameter, project_version_id = project_version_id,
                                                      type = type, pb = NULL, quiet=TRUE, where = .x, split = FALSE)
      )
    if (!is.null(pb)) {
      pb$tick()$print()
    }
    return(results)
  }
  parameter_names <- list_parameter_names(run_id = run_id, project_version_id = project_version_id, include_raw = TRUE) %>%
    dplyr::pull(.data$name)
  filters <- URLencode(.prepare_filter(where, endpoint = 'draws'))
  if (parameter %in% parameter_names) {
    if (filters != '') {
      draws <- geco_api(FIDRAWS, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type, filters=filters)
    } else {
      draws <- geco_api(IDRAWS, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type)
    }
  } else {
    if (filters != '') {
      draws <- geco_api(FIPDRAWS, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type, filters=filters)
    } else {
      draws <- geco_api(IPDRAWS, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type)
    }
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
#' the parameter or predictive quantity to query.
#'
#' The returned object is a data.frame with one record per run, parameter, combination of parameter indices, and quantile value.
#'
#' The data.frame has columns: `quantile`, `.variable`, `run_id`, and `.value` columns, along with the dimensions over which the
#' parameter is estimated such as `subject`, `survival_time`, or `study`.
#'
#' Use \code{\link{format_quantiles_as_widths}} to convert this to a format mimicking the format used by \link[tidybayes:tidybayes-package]{tidybayes}.
#'
#' Posterior quantiles are returned by default. The prior quantiles can be accessed by setting
#' the `type` argument to `prior`.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' @note
#' The quantile probabilites are set to 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, and 0.95. Other quantile
#' probabilities can be computed directly from the draws, if necessary.
#'
#' @note
#' The parameters or predictive quantities for a particular model run can be found by calling
#' \code{\link{list_parameter_names}} or \code{\link{list_predictive_names}}.
#'
#' @note
#' A project can be specified by using the project name or a specific project version.
#' \enumerate{
#'   \item If a project is specified using the name, data is fetched for the latest version of the project.
#'   \item If a project is specified using the project version, the project name is not required.
#'   \item If neither a project nor a project version is provided, the default project or project version is used. These are set by the environment variables GECO_API_PROJECT and GECO_API_PROJECT_VERSION
#' }
#'
#' @note
#' Both the parameter & run_id arguments are vectorized. In the case of multiple values,
#' the rows returned for each run and/or parameter are concatenated into a single data.frame. This is
#' intended for use where the runs use the same model, and parameters are similar dimension.
#'
#' @param parameter (str) [required] Name(s) of the parameters or predictive quantities to be retrieved.
#'                  See \code{\link{list_parameter_names}} and \code{\link{list_predictive_names}}.
#' @param run_id (str) [required] Run id(s). See \code{\link{find_runs}} to see runs available for this project.
#' @param project (str) Project name. If NULL, defaults to value of environment variable GECO_API_PROJECT
#' @param project_version_id (str) Project version. If NULL, defaults to the most recent version of the project if provided, or the value of environment variable GECO_API_PROJECT_VERSION
#' @param type (str) Type of quantile to return, either posterior or prior. Default is `posterior`. To access
#'             prior quantiles, set this to `prior`.
#' @param quiet (bool) if TRUE, suppress informative messages
#' @param .dots (lists) advanced feature used to filter quantiles returned (alternate version of providing ...)
#' @param ... (lists) advanced feature used to filter quantiles returned.
#'     Example: quantile = c(0.5) to limit results to the median, rather than return all quantiles.
#'     Example: trial_arm = unique(subjects$trial_arm_id) to limit quantiles to a set of trial arms, for a parameter estimated per trial arm
#'     However, use of this feature requires that one be familiar with both the dimensions for a particular parameter and their names server-side.
#' @return `data.frame` of quantiles in long format with `quantile`, `.variable`, and `.value` columns
#'         for the 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, and 0.95 quantile probabilities.
#'
#' @seealso  \code{\link{fetch_draws}}, \code{\link{list_parameter_names}}, \code{\link{list_predictive_names}}, \code{\link{format_quantiles_as_widths}}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!! list2
#' @import checkmate
#' @export
fetch_quantiles <- function(parameter, run_id, project = NULL, project_version_id = NULL, type = c('posterior', 'prior'), quiet = FALSE,
                            .dots = list(), ...) {
  extra_dots <- rlang::list2(...)
  filters <- purrr::list_modify(.dots, !!!extra_dots)
  filters <- .check_format(filters, alert = TRUE)
  type <- match.arg(type, several.ok = F)
  checkmate::assert_character(parameter, unique = TRUE)
  checkmate::assert_character(run_id, unique = TRUE)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)

  queries <- purrr::cross2(run_id, parameter)
  show_progress <- length(queries) > 5 && interactive()
  if (isTRUE(show_progress)) {
    pb <- dplyr::progress_estimated(length(queries))
    if (isFALSE(quiet)) {
      futile.logger::flog.info(glue::glue('Querying {type} quantiles for {length(queries)} run_idXparameter combinations.'))
    }
  } else {
    pb <- NULL
  }
  queries %>%
    purrr::map_dfr(~ .fetch_quantiles_per_parameter_run(run_id = .x[[1]], parameter = .x[[2]],
                                                    project_version_id=pv_id,
                                                    type=type,
                                                    pb=pb, quiet=quiet, where = filters))
}

.fetch_quantiles_per_parameter_run <- function(run_id, parameter, project_version_id, type, pb = NULL, quiet = FALSE, where = list(), split = TRUE) {
  if (is.null(pb) && interactive() && isFALSE(quiet)) {
    futile.logger::flog.info(glue::glue('Querying {type} quantiles of {parameter} from run {run_id}.'))
  }
  if (isTRUE(split)) {
    split_filter <- .split_filter(where)
    results <- split_filter %>%
      purrr::map_dfr(~ .fetch_quantiles_per_parameter_run(run_id = run_id, parameter = parameter, project_version_id = project_version_id,
                                                          type = type, pb = NULL, quiet=TRUE, where = .x, split = FALSE)
      )
    if (!is.null(pb)) {
      pb$tick()$print()
    }
    return(results)
  }
  parameter_names = list_parameter_names(run_id = run_id, project_version_id = project_version_id, include_raw = TRUE) %>%
    dplyr::pull(.data$name)
  filters <- URLencode(.prepare_filter(where, endpoint = 'draws'))
  if (parameter %in% parameter_names) {
    if (filters != '') {
      quantiles <- geco_api(FITILES, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type, filters = filters)
    } else {
      quantiles <- geco_api(ITILES, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type)
    }
  } else {
    if (filters != '') {
      quantiles <- geco_api(FIPTILES, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type, filters=filters)
    } else {
      quantiles <- geco_api(IPTILES, project_version_id = project_version_id, run_id=run_id, parameter=parameter, type=type)
    }
  }
  q <- convert_xarray_to_df(quantiles, name = parameter) %>%
    dplyr::mutate(run_id = run_id)
  if (nrow(q) > 0) {
    q <- q %>%
      dplyr::arrange(.data$quantile)
  }
  q
}
