
#' Fetch event data from the Generable API
#'
#' Fetch event data from the Generable API for a specific project.
#'
#' This function retrieves event data from the Generable API.
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
#' @param event_type Limits the event_types to the names provided. Example: "overall_survival".
#'                   NULL is unfiltered. Default is NULL.
#' @return data.frame with one record per subject and event type
#' @export
fetch_events <- function(project = NULL, project_version_id = NULL, event_type = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  events <- .fetch_events_data(project_version_id = pv_id)
  if (!is.null(event_type)) {
    events <- events %>%
      dplyr::filter(.data$event_type %in% !!event_type)
  }
  if (nrow(events) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No event information available for this version of project {project} data.'))
  } else if (nrow(events) == 0) {
    futile.logger::flog.debug(glue::glue('No event information available for this project_version_id: {project_version_id}.'))
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
.fetch_events_data <- function(project = NULL, project_version_id = NULL) {
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



