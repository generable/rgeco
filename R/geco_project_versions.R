
#' Lists project versions within a project
#'
#' @param project (str) the project id. If NULL, defaults to value of environment variable GECO_API_PROJECT
#'
#' @note
#' The default project can set via ENV variable: GECO_API_PROJECT. Inputs given here will
#' override the default value.
#'
#' Lists project versions and provides information about each
#' project version
#'
#' This function fetches the list of project versions within the specified
#' project from the Generable API. Included are the
#' project version id, description, and other information pertient to
#' the project version
#'
#' @return data.frame containing information about each available project version,
#'    one project version per row.
#' @export
list_project_versions <- function(project = NULL) {
  project_versions <- .list_project_version_data(project = project)
  return(
    project_versions %>%
      dplyr::arrange(.data$created_at)
  )
}

.list_project_version_data <- function(project = NULL) {
  if (is.null(project)) {
    futile.logger::flog.info('Project not provided. Using ENV variable: GECO_API_PROJECT.')
    project <- .get_project()
  }
  versions <- geco_api(PROJECTVERSIONS, project = project)
  pv <- as_dataframe.geco_api_data(versions)
}

get_latest_version_id <- function(project = NULL) {
  get_latest_version(project)$id
}

get_latest_version <- function(project = NULL) {
  v <- list_project_versions(project = project)
  v %>%
    dplyr::filter(.data$created_at == max(.data$created_at)) %>%
    as.list()
}
