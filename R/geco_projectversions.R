
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
fetch_projectversions <- function(project) {
  projectversions <- .fetch_projectversion_data(project = project)
  return(projectversions)
}

.fetch_projectversion_data <- function(project) {
  versions <- geco_api(PROJECTVERSIONS, project = project)
  pv <- as_dataframe.geco_api_data(versions)
}
