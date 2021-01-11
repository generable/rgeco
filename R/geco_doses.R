
#' Get dosing data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @return data.frame of biomarkers data
#' @export
get_geco_doses <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  doses <- .get_geco_dose_data(project_version_id = pv_id)
}

.get_geco_dose_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  doses <- geco_api(DOSE, project_version_id = pv_id)
  d <- as_dataframe.geco_api_data(doses, flatten_names = c('drug', 'params')) %>%
    dplyr::rename_at(.vars = dplyr::vars(created_at, dplyr::matches('params'), id), .funs = ~ stringr::str_c('dose_', .x))
}
