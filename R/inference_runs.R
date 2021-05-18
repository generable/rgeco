
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms
fetch_inference_runs <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  ret <- geco_api(IRUNS, project_version_id = pv_id)
  if (length(ret$content) > 0) {
    d <- ret$content %>%
      purrr::map(purrr::map_if, is.list, ~ list(purrr::flatten(.x))) %>%
      purrr::map_dfr(tibble::as_tibble_row)
    # convert run_started_at into date-time field
    if ('run_started_on' %in% names(d)) {
      d <- d %>%
        dplyr::mutate(run_start_datetime = lubridate::ymd_hms(.data$run_started_on))
    }
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(-dplyr::starts_with('run_'), -dataset_id, -model_id),
                       .funs = ~ stringr::str_c('run_', .x))
  } else {
    d <- tibble::tibble(run_id = character(0))
    futile.logger::flog.info('No runs returned.')
  }
  d
}


