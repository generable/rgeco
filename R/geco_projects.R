
#' Lists available projects
#'
#' Lists projects available to the authenticated user.
#'
#' This function fetches the list of projects available to the
#' authenticated user from the Generable API. Included are the
#' project id, description, and other information pertient to
#' the project.
#'
#' @return data.frame containing information about each available project,
#'    one project per row.
#' @export
fetch_projects <- function() {
  projects <- .fetch_projects_data()
  return(projects)
}

.fetch_projects_data <- function() {
  projects <- geco_api(PROJECTS)
  p <- as_dataframe.geco_api_data(projects, flatten_names = c())
}
