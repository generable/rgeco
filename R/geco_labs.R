
#' Fetch lab data from the Generable API
#'
#' Fetch lab data from the Generable API for a specific project.
#'
#' This function retrieves labs data from the Generable API.
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
#' @param annotate if `TRUE`, annotate lab data with dose data. Default is `TRUE`.
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @return data.frame of lab data for the project specified
#' @export
fetch_labs <- function(project = NULL, project_version_id = NULL, annotate = T) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  d <- .fetch_labs_data(project_version_id = pv_id, annotate = annotate)
  if (nrow(d) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No labs information available for this version of project {project} data.'))
  } else if (nrow(d) == 0) {
    futile.logger::flog.debug(glue::glue('No biomarkers information available for this project_version_id: {project_version_id}.'))
  }
  d
}

#' @importFrom magrittr %>%
.fetch_labs_data <- function(project = NULL, project_version_id = NULL, annotate = T) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  labs <- geco_api(LABS, project_version_id = pv_id)
  d <- as_dataframe.geco_api_data(labs, flatten_names = 'params')
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'params', 'id', 'description'))),
                       .funs = ~ stringr::str_c('lab_', .x))
  })
  if (isTRUE(annotate) && nrow(d) > 0) {
    if ('time' %in% names(d)) {
      d <- d %>%
        dplyr::rename(trial_day = .data$time)
    }
  }
  d
}
