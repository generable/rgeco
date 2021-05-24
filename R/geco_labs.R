
#' Fetch lab data from the Generable API
#'
#' Fetch lab data from the Generable API for a specific project.
#'
#' This function retrieves biomarker data from the Generable API.
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
    if ('lab_params' %in% names(d) && ncol(d$lab_params) > 0) {
      d <- dplyr::bind_cols(d, d$lab_params) %>%
        dplyr::select(-.data$lab_params)
    }
    if (!'time' %in% names(d)) {
      d <- d %>%
        dplyr::rename(trial_day = .data$time)
    }
  }
  d
}
