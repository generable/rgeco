
#' Fetch biomarker data from the Generable API
#'
#' Fetch biomarker data from the Generable API for a specific Generable project.
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
#' @param project Project name
#' @param project_version_id Project version. If this is specified, the `project` argument is ignored.
#' @param measurement_name Vector of measurement names to return. `NULL` returns all measurements. Default
#'        is `NULL`.
#' @param annotate if `TRUE`, annotate biomarker data with dose data. Default is `TRUE`.
#' @param annotate_doses if `TRUE`, annotate biomarker data with timing of dose administrations.
#'                       Default is `TRUE`.
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @return data.frame of biomarkers data for the project specified
#' @export
fetch_biomarkers <- function(project = NULL, project_version_id = NULL, measurement_name = NULL, annotate = T, annotate_doses = T) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  biomarkers <- .fetch_timevarying_data(project_version_id = pv_id, annotate = annotate, measurement_name = measurement_name)
  if (!is.null(measurement_name)) {
    biomarkers <- biomarkers %>%
      dplyr::filter(measurement_name %in% !!measurement_name)
  } else {
    biomarkers
  }
  if (isTRUE(annotate_doses)) {
    # try to annotate with dose data, if available
    dose_data <- try(fetch_doses(project_version_id = pv_id), silent = T)
    if (!inherits(dose_data, 'try-error') && !is.null(dose_data) && nrow(dose_data) > 0) {
      biomarkers <- prep_pkpd_data(biomarkers_data = biomarkers, dose_data = dose_data, pd_measure = NULL, pk_measure = NULL)
    }
  }
  if (nrow(biomarkers) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No biomarkers information available for this version of project {project} data.'))
  } else if (nrow(biomarkers) == 0) {
    futile.logger::flog.debug(glue::glue('No biomarkers information available for this project_version_id: {project_version_id}.'))
  }
  biomarkers
}

#' @importFrom magrittr %>%
.fetch_timevarying_data <- function(project = NULL, project_version_id = NULL, annotate = T, measurement_name = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (!is.null(measurement_name)) {
    measurement_names <- stringr::str_c(measurement_name, collapse = ',')
    biomarkers <- geco_api(TIMEVARYING, project_version_id = pv_id, query = list(measurement_name = measurement_names))
  } else {
    biomarkers <- geco_api(TIMEVARYING, project_version_id = pv_id)
  }
  b <- as_dataframe.geco_api_data(biomarkers, flatten_names = 'params')
  suppressWarnings({
    b <- b %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'params', 'id'))),
                       .funs = ~ stringr::str_c('measurement_', .x))
  })
  if (isTRUE(annotate) && nrow(b) > 0) {
    if ('measurement_params' %in% names(b) && ncol(b$measurement_params) > 0) {
      b <- dplyr::bind_cols(b, b$measurement_params) %>%
        dplyr::select(-.data$measurement_params)
    }
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

#' Fetch distinct list of measurement names for a project
#'
#' @param project Project name
#' @param project_version_id Project version. If this is specified, the `project` argument is ignored.
#' @return vector of measurement names where there is at least one observation in the proejct
#' @export
fetch_measurement_names <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  b <- geco_api(TIMEVARYING, project_version_id = pv_id, add_headers(`X-Fields` = 'measurement_name')) %>%
    as_dataframe.geco_api_data()
  b <- b %>% dplyr::distinct(.data$measurement_name) %>% unlist() %>% purrr::set_names(NULL)
  if (length(b) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No measurement names available for this version of project {project} data.'))
  } else if (length(b) == 0) {
    futile.logger::flog.debug(glue::glue('No measurement names available for this project_version_id: {project_version_id}.'))
  }
  b
}
