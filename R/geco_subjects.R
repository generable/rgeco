
#' Fetch subjects data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @param event_type (chr) Optionally limit event_types to the names provided (example: "overall_survival"). The default (NULL) is to include no event data.
#' @param annotate (bool) if TRUE, return annotated (processed, formatted) subjects data
#' @return data.frame of subject-level data, including information about the trial & trial_arms
#' @export
fetch_subjects <- function(project = NULL, project_version_id = NULL, event_type = NULL, annotate = T) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  subjects <- .fetch_subjects_data(project_version_id = pv_id)
  if (!is.null(event_type)) {
    events <- fetch_events(project_version_id = pv_id, event_type = event_type) %>%
      pivot_events_wider()
    subjects <- subjects %>%
      dplyr::left_join(events, by = 'subject_id')
  }
  trial_arms <- .fetch_trial_arms_data(project_version_id = pv_id)
  trials <- .fetch_trials_data(project_version_id = pv_id)
  s <- subjects %>%
    dplyr::left_join(trial_arms,
                     by = c('trial_arm_id'), suffix = c('', '_trial_arm')) %>%
    dplyr::left_join(trials,
                     by = c('trial_id'), suffix = c('', '_trial'))
  if ('subject_params' %in% names(s)) {
    s <- dplyr::bind_cols(s, s$subject_params) %>%
      dplyr::select(-.data$subject_params)
  }
  if (isTRUE(annotate)) {
    s <- .annotate_subjects_data(s)
  }
  if (nrow(s) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No subject information available for this version of project {project} data.'))
  } else if (nrow(s) == 0) {
    futile.logger::flog.debug(glue::glue('No subject information available for this project_version_id: {project_version_id}.'))
  }
  s
}

.fetch_subjects_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  subjects <- geco_api(SUBJECTS, project_version_id = pv_id)
  s <- as_dataframe.geco_api_data(subjects, flatten_names = 'params')
  suppressWarnings({
    s <- s %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of('created_at', 'params', 'id')),
                       .funs = ~ stringr::str_c('subject_', .x))
  })
  s
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
