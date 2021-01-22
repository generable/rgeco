
#' Get listing of available GECO projects
#' @return data.frame of project-level data, including information about the project scope
#' @export
fetch_projects <- function() {
  projects <- .fetch_projects_data()
}

.fetch_projects_data <- function() {
  projects <- geco_api(PROJECTS)
  p <- as_dataframe.geco_api_data(projects, flatten_names = c())
}
