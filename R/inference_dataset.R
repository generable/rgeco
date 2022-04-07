
#' List datasets from the Generable API
#'
#' List datasets from the Generable API for a specific project.
#'
#' A `dataset` is used by a model to generate a run. This function retrieves
#' the metadata about all datasets within a project version.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the metadata from the Generable API.
#'
#' @note
#' A project can be specified by using the project name or a specific project version.
#' \enumerate{
#'   \item If a project is specified using the name, data is fetched for the latest version of the project.
#'   \item If a project is specified using the project version, the project name is not required.
#'   \item If neither a project nor a project version is provided, the default project or project version is used. These are set by the environment variables GECO_API_PROJECT and GECO_API_PROJECT_VERSION
#' }
#'
#' @param project Project name. If NULL, defaults to value of environment variable GECO_API_PROJECT
#' @param project_version_id Project version. If NULL, defaults to the most recent version of the project if provided, or the value of environment variable GECO_API_PROJECT_VERSION
#' @return data.frame of metadata for all datasets for the project specified
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
list_datasets <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  ret <- geco_api(IDATA, project_version_id = pv_id)
  if (httr::http_error(ret$response)) {
    stop(stringr::str_c('Error querying API: ', ret$response))
  }
  if (length(ret$content) > 0) {
    d <- ret$content %>%
      purrr::map(purrr::compact) %>%
      purrr::map(purrr::map_if, ~ is.list(.x) & length(.x) > 1, ~ list(.x)) %>%
      purrr::map_dfr(tibble::as_tibble_row)
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(-dplyr::starts_with('dataset_')),
                       .funs = ~ stringr::str_c('dataset_', .x))
  } else {
    d <- tibble::tibble(dataset_id = character(0))
    futile.logger::flog.info('No analysis datasets returned.')
  }
  d
}

#' Characterize sampling information given dataset info, using standard column locations
#'
#' @importFrom tidyr hoist
#' @param d data.frame containing result of `list_datasets`
#' @return data.frame with new columns containing information about the sample of data generated.
#' @export
extract_subsample_info <- function(d) {
  d %>%
    tidyr::hoist(.data$dataset_params,
                 sample_n = c('sampling_scheme', 'n'),
                 truncate_min_days = c('sampling_scheme', 'min_days'),
                 truncate_max_days = c('sampling_scheme', 'max_days'),
                 sample_id = c('seed_subjects')) %>%
    dplyr::mutate(sample_id = ifelse(is.na(.data$sample_n), 0, .data$sample_id))
}


#' Fetch the dataset from the Generable API for a model run
#'
#' Fetch the dataset from the Generable API for a model run.
#'
#' A `dataset` is used by a model to generate a run. This function retrieves
#' the dataset used by a particular run. This returns the subjects, biomarkers,
#' and other information as a list of data.frames.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the metadata from the Generable API.
#'
#' @note
#' A project can be specified by using the project name or a specific project version.
#' \enumerate{
#'   \item If a project is specified using the name, data is fetched for the latest version of the project.
#'   \item If a project is specified using the project version, the project name is not required.
#'   \item If neither a project nor a project version is provided, the default project or project version is used. These are set by the environment variables GECO_API_PROJECT and GECO_API_PROJECT_VERSION
#' }
#'
#' @param run_id Run id; required.
#' @param project Project name. If NULL, defaults to value of environment variable GECO_API_PROJECT
#' @param project_version_id Project version. If NULL, defaults to the most recent version of the project if provided, or the value of environment variable GECO_API_PROJECT_VERSION
#' @return named list of `data.frame`s
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
fetch_dataset <- function(run_id, project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  resp <- geco_api(IRUNDATA, project_version_id = pv_id, run_id=run_id)
  d <- .format_inference_data(resp$content)
  d
}

.format_result <- function(results, name) {
  if (name == 'biomarkers') {
    .format_data(results, numeric_fields = c('measurement_value'))
  } else if (name == 'subjects') {
    .format_data(results)
  } else {
    .format_data(results)
  }
}

.discover_numeric_fields <- function(results) {
  contains_numeric <- results %>%
    purrr::transpose() %>%
    purrr::map_depth(2, ~ is.na(.x) | is.numeric(.)) %>%
    purrr::map_lgl(~ all(unlist(.x)))
  names(contains_numeric)[contains_numeric]
}

.format_data <- function(results, numeric_fields = c()) {
  numeric_fields <- c(numeric_fields, .discover_numeric_fields(results))
  results <- results %>%
    purrr::map(tibble::as_tibble)
  if (length(numeric_fields) > 0)
    results <- results %>%
    purrr::map(dplyr::mutate_at, .vars = dplyr::vars(dplyr::one_of(numeric_fields)), .funs = as.double)
  dplyr::bind_rows(results)
}

.format_inference_data <- function(content) {
  results <- content %>%
    purrr::map('data') %>%
    purrr::set_names(purrr::map_chr(content, 'table_name')) %>%
    purrr::compact() %>%
    purrr::imap(.f = ~ .format_result(results = .x, name = .y))
}

