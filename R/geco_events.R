
#' Fetch event data from the Generable API
#'
#' Fetch event data from the Generable API for a specific project.
#'
#' This function retrieves event data from the Generable API.
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
#' @param event_type Limits the event_types to the names provided. Example: "overall_survival".
#'                   NULL is unfiltered. Default is NULL.
#' @param ... Optional filters applied to events data, provided as name-value pairs to limit returned values.
#'      Example: trial_id = unique(subjects$trial_id)
#' @return data.frame with one record per subject and event type
#' @export
fetch_events <- function(project = NULL, project_version_id = NULL, event_type = NULL, ...) {
  where <- rlang::list2(...)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (!is.null(event_type)) {
    where <- .update_filter(where, event_type = event_type)
  }
  events <- .fetch_events_data(project_version_id = pv_id, where = where)
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
.fetch_events_data <- function(project = NULL, project_version_id = NULL, where = list()) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  filter <- .prepare_filter(where, endpoint = 'EVENTS')
  events <- geco_api(EVENTS, project_version_id = pv_id, url_query_parameters = filter)
  d <- as_dataframe.geco_api_data(events, flatten_names = c('params'))
  if ('params' %in% names(d)) {
    d <- d %>%
      tidyr::unnest_wider(.data$params)
  }
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'params', 'name', 'phase', 'internal_id', 'id', 'trial_day'))),
                       .funs = ~ stringr::str_c('event_', .x))
  })
  .apply_filters(d, where)
}



