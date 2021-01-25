
#' Fetch dosing data for a Generable project
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @return data.frame of biomarkers data
#' @export
fetch_doses <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  doses <- .fetch_dose_data(project_version_id = pv_id)
  if (nrow(doses) == 0 && !is.null(project)) {
    futile.logger::flog.info(glue::glue('No dosing information available for this version of project {project} data.'))
  } else if (nrow(doses) == 0) {
    futile.logger::flog.debug(glue::glue('No dosing information available for this project_version_id: {project_version_id}.'))
  }
  doses
}

#' @importFrom magrittr %>%
.fetch_dose_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  doses <- geco_api(DOSE, project_version_id = pv_id)
  d <- as_dataframe.geco_api_data(doses, flatten_names = c('drug', 'params'))
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of('created_at', 'id', 'params')),
                       .funs = ~ stringr::str_c('dose_', .x))
  })
  if (nrow(d) > 0) {
    d <- d %>%
      dplyr::mutate(start_hours = .format_hours(.data$trial_day, .data$start_time),
                    end_hours = .format_hours(.data$trial_day, .data$end_time)) %>%
      dplyr::group_by(.data$subject_id) %>%
      dplyr::mutate(cycle_num = dplyr::dense_rank(.data$start_hours)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(administered = factor(stringr::str_c(.data$amount, .data$unit)),
                    administered = forcats::fct_reorder(.data$administered, .data$amount),
                    cycle = factor(stringr::str_c('Cycle ', .data$cycle_num)),
                    cycle = forcats::fct_reorder(.data$cycle, .data$cycle_num))
  }
  d
}

.format_hours <- function(trial_day, time_str) {
  checkmate::assert_integerish(trial_day)
  checkmate::assert_character(time_str, pattern = '\\d{2}\\:\\d{2}\\:\\d{2}', len = length(trial_day))
  day_hours <- trial_day * 24
  time_str <- dplyr::if_else(is.na(time_str),
                             '00:00:00', time_str)
  time_hours <- as.numeric(lubridate::hms(time_str), units = 'hours')
  day_hours + time_hours
}
