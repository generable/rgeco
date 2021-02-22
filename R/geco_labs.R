
#' Fetch labs data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @param annotate (bool) if TRUE, annotate returned biomarker data
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @return data.frame of labs data
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
        dplyr::rename(trial_day = time)
    }
  }
  d
}
