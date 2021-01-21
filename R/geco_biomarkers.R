
#' Get biomarkers data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @param measurement_name (chr, vector) Optionally, a list of measurement names to return
#' @param annotate (bool) if TRUE, annotate returned biomarker data
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @return data.frame of biomarkers data
#' @export
get_geco_biomarkers <- function(project = NULL, project_version_id = NULL, measurement_name = NULL, annotate = T) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  biomarkers <- .get_geco_timevarying_data(project_version_id = pv_id, annotate = annotate)
  if (!is.null(measurement_name)) {
    biomarkers %>%
      dplyr::filter(measurement_name %in% !!measurement_name)
  } else {
    biomarkers
  }
}

#' @importFrom magrittr %>%
.get_geco_timevarying_data <- function(project = NULL, project_version_id = NULL, annotate = T) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  biomarkers <- geco_api(TIMEVARYING, project_version_id = pv_id)
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
    if (all(c('trial_day', 'time') %in% names(b))) {
      b <- b %>%
        dplyr::mutate(hours = .format_hours(.data$trial_day, .data$time))
    }
  }
  b
}
