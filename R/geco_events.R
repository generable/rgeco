
#' Get event data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @return data.frame with one record per subject and event type
#' @export
get_geco_events <- function(project = NULL, project_version_id = NULL, event_type = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  events <- .get_geco_events_data(project_version_id = pv_id)
  if (!is.null(event_type)) {
    events <- events %>%
      dplyr::filter(.data$event_type %in% !!event_type)
  }
  events
}


#' @importFrom rlang .data
pivot_events_wider <- function(.d) {
  tidyr::pivot_wider(.d,
                     id_cols = c(.data$subject_id),
                     names_from = c(.data$event_type),
                     values_from = c(.data$event_flag, .data$event_trial_day, .data$event_id, .data$event_created_at),
                     names_glue = "{event_type}_{.value}")
}


#' @importFrom magrittr %>%
.get_geco_events_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  events <- geco_api(EVENTS, project_version_id = pv_id)
  d <- as_dataframe.geco_api_data(events, flatten_names = c())
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'params', 'name', 'phase', 'internal_id', 'id', 'trial_day'))),
                       .funs = ~ stringr::str_c('event_', .x))
  })
  d
}



