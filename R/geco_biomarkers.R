
#' Fetch biomarker data from the Generable API
#'
#' Fetch biomarker data from the Generable API for a specific project.
#'
#' This function retrieves biomarker data from the Generable API.
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
#' @param measurement_name Vector of measurement names to return. `NULL` returns all measurements. Default
#'        is `NULL`.
#' @param annotate if `TRUE`, annotate biomarker data with dose data. Default is `TRUE`.
#' @param annotate_doses if `TRUE`, annotate biomarker data with timing of dose administrations.
#'                       Default is `TRUE`.
#' @param ... Optional filters applied to biomarkers data, provided as name-value pairs to limit returned values.
#'      Example: trial_id = unique(subjects$trial_id)
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @return data.frame of biomarkers data for the project specified
#' @export
fetch_biomarkers <- function(project = NULL, project_version_id = NULL, measurement_name = NULL, annotate = T, annotate_doses = T, ...) {
  where <- rlang::list2(...)
  where <- .check_format(where, alert = T)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  biomarkers <- .fetch_timevarying_data(project_version_id = pv_id, annotate = annotate, measurement_name = measurement_name, where = where)
  if (isTRUE(annotate_doses)) {
    # try to annotate with dose data, if available
    dose_data <- try(.fetch_dose_data(project_version_id = pv_id, where = where), silent = T)
    if (!inherits(dose_data, 'try-error') && !is.null(dose_data) && nrow(dose_data) > 0) {
      biomarkers <- prep_pkpd_data(biomarkers_data = biomarkers, dose_data = dose_data, pd_measure = NULL, pk_measure = NULL)
    }
  }
  if (nrow(biomarkers) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No biomarkers information available for this version of project {project} data.'))
  } else if (nrow(biomarkers) == 0) {
    futile.logger::flog.debug(glue::glue('No biomarkers information available for this project_version_id: {project_version_id}.'))
  }
  biomarkers
}

#' @importFrom magrittr %>%
.fetch_timevarying_data <- function(project = NULL, project_version_id = NULL, annotate = T, measurement_name = NULL, where = list()) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  if (!is.null(measurement_name)) {
    where <- .update_filter(where, measurement_name = measurement_name)
  }
  filters <- .prepare_filter(where, endpoint = 'TIMEVARYING')
  if (length(names(where))>length(names(filters))) {
    cli::cli_alert_info(glue::glue('Not all filters applied will operate server-side. The most efficient queries will filter on {glue::glue_collapse(.get_supported_keys("TIMEVARYING"), sep=", ", last = ", or ")}'))
  }
  biomarkers <- geco_api(TIMEVARYING, project_version_id = pv_id, url_query_parameters = filters)
  b <- as_dataframe.geco_api_data(biomarkers, flatten_names = 'params')
  if (nrow(b) > 0 && 'params' %in% names(b) && isTRUE(annotate)) {
    b <- b %>% tidyr::unnest_wider(.data$params, names_repair = 'universal')
  }
  suppressWarnings({
    b <- b %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(c('created_at', 'id'))),
                       .funs = ~ stringr::str_c('measurement_', .x))
  })
  if (isTRUE(annotate) && nrow(b) > 0) {
    if (!'time' %in% names(b)) {
      b <- b %>%
        dplyr::mutate(time = NA_character_)
    }
    if (all(c('trial_day', 'time') %in% names(b))) {
      b <- b %>%
        dplyr::mutate(hours = .format_hours(.data$trial_day, .data$time))
    }
  }
  .apply_filters(b, where)
}

#' List distinct biomarker measurement names for a project
#'
#' @param project Project name
#' @param project_version_id Project version. If this is specified, the `project` argument is ignored.
#' @return vector of measurement names where there is at least one observation in the proejct
#' @export
list_biomarker_names <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  b <- geco_api(TIMEVARYING, project_version_id = pv_id, add_headers(`X-Fields` = 'measurement_name')) %>%
    as_dataframe.geco_api_data()
  b <- b %>% dplyr::distinct(.data$measurement_name) %>% unlist() %>% purrr::set_names(NULL)
  if (length(b) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No measurement names available for this version of project {project} data.'))
  } else if (length(b) == 0) {
    futile.logger::flog.debug(glue::glue('No measurement names available for this project_version_id: {project_version_id}.'))
  }
  b
}
