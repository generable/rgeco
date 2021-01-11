
#' Get subjects data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @return data.frame of subject-level data, including information about the trial & trial_arms
#' @export
get_geco_subjects <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  subjects <- .get_geco_subjects_data(project_version_id = pv_id)
  trial_arms <- .get_geco_trial_arms_data(project_version_id = pv_id)
  trials <- .get_geco_trials_data(project_version_id = pv_id)
  s <- subjects %>%
    dplyr::left_join(trial_arms,
                     by = c('trial_arm_id'), suffix = c('', '_trial_arm')) %>%
    dplyr::left_join(trials,
                     by = c('trial_id'), suffix = c('', '_trial'))
}

.get_geco_subjects_data <- function(project = NULL, project_version_id = NULL) {
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
