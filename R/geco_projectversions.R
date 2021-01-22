
#' Get information about versions available for a project
#' @param project (chr) project id for which version information is requested
#' @return data.frame with information about each project-version available for a project
#' @export
fetch_projectversions <- function(project) {
  projectversions <- .fetch_projectversion_data(project = project)
  return(projectversions)
}

.fetch_projectversion_data <- function(project) {
  versions <- geco_api(PROJECTVERSIONS, project = project)
  pv <- as_dataframe.geco_api_data(versions)
}
