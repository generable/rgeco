
#' Fetch subject data from the Generable API
#'
#' Fetch subject data from the Generable API for a specific project.
#'
#' This function retrieves subject data from the Generable API. This includes
#' subject-level covariates.
#'
#' As a convenience, event data can be included in the returned `data.frame` by
#' specifying the `event_type` argument; the data returned from \code{\link{fetch_events}}
#' matching the `event_type` will be merged into the returned data.
#'
#' Authentication (see \code{\link{login}}) is reqiured prior to use
#' and this pulls subject data from the Generable API.
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
#' @param event_type If this argument is provided, event data that matches this event type will be
#'                   joined into the return data.frame. Default is NULL (no event data).
#' @param annotate if `TRUE`, annotate subject data with dose data. Default is `TRUE`.
#' @param ... Optional filters applied to subjects data, provided as name-value pairs where names are fields and values contain a subset of valid values
#'      Example: trial_name = c('trial-A', 'trial-N'), performance_status = c(0,1)
#' @return data.frame of subject-level data, including information about the trial and trial_arms
#' @export
fetch_subjects <- function(project = NULL, project_version_id = NULL, event_type = NULL, annotate = T, ...) {
  where <- rlang::list2(...)
  where <- .check_format(where, alert = T)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  trials <- .fetch_trials_data(project_version_id = pv_id, where = where)
  if (nrow(trials) == 0) {
    return(tibble::tibble())
  }
  active_filter <- .update_filter(where, trial_id = unique(trials$trial_id))
  trial_arms <- .fetch_trial_arms_data(project_version_id = pv_id, where = active_filter)
  if (nrow(trial_arms) == 0) {
    return(tibble::tibble())
  }
  active_filter <- .update_filter(active_filter, trial_arm_id = unique(trial_arms$trial_arm_id))
  s <- .fetch_subjects_data(project_version_id = pv_id, where = active_filter)
  if (!is.null(event_type)) {
    active_filter <- .update_filter(active_filter, event_type = event_type)
    events <- .fetch_events_data(project_version_id = pv_id, where = active_filter) %>%
      pivot_events_wider()
    s <- s %>%
      dplyr::left_join(events, by = 'subject_id')
  }
  if (nrow(s) > 0) {
    s <- s %>%
      dplyr::left_join(trial_arms,
                       by = c('trial_arm_id'), suffix = c('', '_trial_arm')) %>%
      dplyr::left_join(trials,
                       by = c('trial_id'), suffix = c('', '_trial'))
    if ('subject_params' %in% names(s)) {
      s <- s %>%
        tidyr::unnest_wider(.data$subject_params)
    }
    if (isTRUE(annotate)) {
      s <- .annotate_subjects_data(s)
    }
  }
  if (nrow(s) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No subject information available for this version of project {project} data.'))
  } else if (nrow(s) == 0) {
    futile.logger::flog.debug(glue::glue('No subject information available for this project_version_id: {project_version_id}.'))
  }
  s
}

.fetch_subjects_data <- function(project = NULL, project_version_id = NULL, where = list()) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  filter <- .prepare_filter(where, endpoint = 'SUBJECTS')
  subjects <- geco_api(SUBJECTS, project_version_id = pv_id, url_query_parameters = filter)
  s <- as_dataframe.geco_api_data(subjects, flatten_names = 'params')
  suppressWarnings({
    s <- s %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of('created_at', 'params', 'id')),
                       .funs = ~ stringr::str_c('subject_', .x))
  })
  .apply_filters(s, where)
}

.annotate_subjects_data <- function(s) {
  if (all(c('age_min', 'age_max') %in% names(s))) {
    s <- s %>%
      dplyr::mutate(age = (.data$age_min + .data$age_max) / 2)
  }
  if ('performance' %in% names(s)) {
    s <- s %>%
      dplyr::mutate(performance = factor(.data$performance, levels = c('fully_active', 'restricted_activity', 'self-care_only'), ordered = T))
  }
  if ('baseline_weight_min' %in% names(s)) {
    s <- s %>%
      dplyr::mutate(baseline_weight = .data$baseline_weight_min + .data$baseline_weight_max / 2)
  }
  if ('indication' %in% names(s)) {
    s <- s %>%
      dplyr::mutate(indication = dplyr::case_when(stringr::str_to_lower(.data$indication) == 'mcrpc' ~ 'mCRPC',
                                                  TRUE ~ .data$indication),
                    indication = factor(.data$indication),
                    indication = forcats::fct_infreq(.data$indication) %>% forcats::fct_explicit_na())
  }
  if ("prior_line_number" %in% names(s)) {
    s <- s %>%
      dplyr::mutate(prior_lines = suppressWarnings(as.integer(.data$prior_line_number)))
  }
  if ('study' %in% names(s)) {
    s <- s %>%
      dplyr::mutate(study = factor(.data$study))
  }
  if ('sex' %in% names(s)) {
    s <- s %>%
      dplyr::mutate(sex = factor(.data$sex))
  }
  # apply friendly labels to fields
  s <- apply_labels(s, .subject_data_labels)
  s
}

.subject_data_labels <- c(
  sex = 'Sex',
  indication = 'Tumor Type',
  age = 'Age (years)',
  dose = 'Dose',
  prior_lines = '# Prior Lines of Therapy (any)',
  performance = 'ECOG at baseline',
  height = 'Height at baseline (cm)',
  weight = 'Weight at baseline (kg)',
  bmi = 'BMI at baseline'
)

utils::globalVariables('.')

apply_labels <- function(.d, labels) {
  labelled_d <- labels %>%
    purrr::reduce2(.init = .d, .x = names(.), .y = ., .apply_single_label)
}

.apply_single_label <- function(.d, varname, label_text) {
  if (varname %in% names(.d))
    table1::label(.d[[varname]]) <- label_text
  .d
}
