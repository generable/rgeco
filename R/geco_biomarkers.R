
#' Get biomarkers data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @param measurement_name (chr, vector) Optionally, a list of measurement names to return
#' @return data.frame of biomarkers data
#' @export
get_geco_biomarkers <- function(project = NULL, project_version_id = NULL, measurement_name = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  biomarkers <- .get_geco_timevarying_data(project_version_id = pv_id)
  if (!is.null(measurement_name)) {
    biomarkers %>%
      dplyr::filter(measurement_name %in% !!measurement_name)
  } else {
    biomarkers
  }
}

.get_geco_timevarying_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  biomarkers <- geco_api(TIMEVARYING, project_version_id = pv_id)
  b <- as_dataframe.geco_api_data(biomarkers, flatten_names = 'params') %>%
    dplyr::rename(measurement_id = id,
                  id = subject_id)
}
