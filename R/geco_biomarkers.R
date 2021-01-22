
#' Fetch biomarkers data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @param measurement_name (chr, vector) Optionally, a list of measurement names to return. NULL returns all measurements
#' @param annotate (bool) if TRUE, annotate returned biomarker data
#' @param annotate_doses (bool) if TRUE, annotated returned biomarker data with timing of dose administrations, if available
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @return data.frame of biomarkers data
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
    if (!inherits(dose_data, 'try-error') && nrow(dose_data) > 0) {
      biomarkers <- prep_pkpd_data(biomarkers_data = biomarkers, dose_data = dose_data)
    }
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
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @return character vector of measurement names with at least one observation
#' @export
fetch_measurement_names <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  b <- geco_api(TIMEVARYING, project_version_id = pv_id, add_headers(`X-Fields` = 'measurement_name')) %>%
    as_dataframe.geco_api_data()
  b %>% distinct(measurement_name) %>% unlist() %>% purrr::set_names(NULL)
}
