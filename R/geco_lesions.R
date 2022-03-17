
#' Fetch per-lesion biomarker data from the Generable API
#'
#' Fetch per-lesion biomarker data from the Generable API for a specific project.
#'
#' This function retrieves lesion-level biomarker data from the Generable API.
#' It requires authentication (see \code{\link{login}}) prior to use
#' and this pulls data from the Generable API.
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
#' @param measurement_name Vector of measurement names to return. `NULL` returns all measurements. Default
#'        is `NULL`.
#' @param annotate whether to format response data & merge with lesion-level characteristics (default: TRUE)
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @return data.frame of lesion-level biomarkers data for the project specified
#' @export
fetch_lesion_biomarkers <- function(project = NULL, project_version_id = NULL, annotate = T, measurement_name = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  biomarkers <- .fetch_timevarying_lesion_data(project_version_id = pv_id, annotate = annotate, measurement_name = measurement_name)
  if (!is.null(measurement_name)) {
    biomarkers <- biomarkers %>%
      dplyr::filter(measurement_name %in% !!measurement_name)
  } else {
    biomarkers
  }
  if (isTRUE(annotate) && nrow(biomarkers)>0) {
    lesions <- .fetch_lesion_data(project_version_id = pv_id, annotate = annotate)
    biomarkers <- biomarkers %>%
      dplyr::left_join(lesions, by = 'lesion_id')
  }
  if (nrow(biomarkers) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No lesion-level biomarkers available for this version of project {project} data.'))
  } else if (nrow(biomarkers) == 0) {
    futile.logger::flog.debug(glue::glue('No lesion-level biomarkers available for this project_version_id: {project_version_id}.'))
  }
  biomarkers
}

#' Fetch lesions data from the Generable API
#'
#' Fetch lesion descriptions from the Generable API for a specific project.
#'
#' This function retrieves lesion-level characteristics from the Generable API.
#' It requires authentication (see \code{\link{login}}) prior to use
#' and this pulls data from the Generable API.
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
#' @param annotate whether to format response data
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @return data.frame of lesions data for the project specified
#' @export
fetch_lesions <- function(project = NULL, project_version_id = NULL, annotate = T) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  lesions <- .fetch_lesion_data(project_version_id = pv_id, annotate = annotate)
  if (nrow(lesions) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No lesions available for this version of project {project} data.'))
  } else if (nrow(lesions) == 0) {
    futile.logger::flog.debug(glue::glue('No lesions available for this project_version_id: {project_version_id}.'))
  }
  lesions
}

#' @importFrom magrittr %>%
.fetch_lesion_data <- function(project = NULL, project_version_id = NULL, annotate = T) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  lesions <- geco_api(LESIONS, project_version_id = pv_id)
  d <- as_dataframe.geco_api_data(lesions, flatten_names = 'params')
  if (nrow(d) > 0 && 'params' %in% names(d) && isTRUE(annotate)) {
    d <- d %>% tidyr::unnest_wider(.data$params)
  }
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'id', 'label'))),
                       .funs = ~ stringr::str_c('lesion_', .x))
  })
  d
}

#' @importFrom magrittr %>%
.fetch_timevarying_lesion_data <- function(project = NULL, project_version_id = NULL, annotate = T, measurement_name = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (!is.null(measurement_name)) {
    measurement_names <- stringr::str_c(measurement_name, collapse = ',')
    biomarkers <- geco_api(LESIONTV, project_version_id = pv_id, query = list(measurement_name = measurement_names))
  } else {
    biomarkers <- geco_api(LESIONTV, project_version_id = pv_id)
  }
  b <- as_dataframe.geco_api_data(biomarkers, flatten_names = 'params')
  if (nrow(b) > 0 && 'params' %in% names(b) && isTRUE(annotate)) {
    b <- b %>% tidyr::unnest_wider(.data$params)
  }
  suppressWarnings({
    b <- b %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'id'))),
                       .funs = ~ stringr::str_c('measurement_', .x))
  })
  if (isTRUE(annotate) && nrow(b) > 0) {
    if (!'time' %in% names(b)) {
      b <- b %>%
        dplyr::mutate(time = NA_character_)
    }
    if (all(c('trial_day', 'time') %in% names(b))) {
      b <- b %>%
        dplyr::mutate(hours = .format_hours(.data$trial_day, .data$time))
    }
  }
  b
}


