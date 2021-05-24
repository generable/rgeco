
#' Lists project versions within a project
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
list_project_versions <- function(project) {
  project_versions <- .list_project_version_data(project = project)
  return(project_versions)
}

.list_project_version_data <- function(project) {
  versions <- geco_api(PROJECTVERSIONS, project = project)
  pv <- as_dataframe.geco_api_data(versions)
}
