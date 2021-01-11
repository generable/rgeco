
#' Get listing of available GECO projects
#' @return data.frame of project-level data, including information about the project scope
#' @export
get_geco_projects <- function() {
  projects <- .get_geco_projects_data()
}

.get_geco_projects_data <- function() {
  projects <- geco_api(PROJECTS)
  p <- as_dataframe.geco_api_data(projects, flatten_names = c())
}
