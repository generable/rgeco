
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms
fetch_inference_runs <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  ret <- geco_api(IRUNS, project_version_id = pv_id)
  if (length(ret$content) > 0) {
    d <- ret$content %>%
      purrr::map_dfr(tibble::enframe, .id = '.id') %>%
      tidyr::spread(.data$name, .data$value) %>%
      dplyr::select_if(.predicate = ~ all(!is.null(unlist(.x)))) %>%
      tidyr::unnest(cols = c(dplyr::one_of('dataset_id', 'model_id', 'started_on', 'id'))) %>%
      dplyr::select(-.data$.id)
      suppressWarnings({
        d <- d %>%
          dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of(c('dataset_id', 'model_id', 'run_args'))),
                           .funs = ~ stringr::str_c('run_', .x))
        # unpack columns that are lists of chrs
        d <- d %>%
          dplyr::mutate_at(.vars = dplyr::vars(dplyr::one_of('run_parameters', 'run_posterior_predictive', 'run_prior_predictive', 'run_priors')),
                           .funs = ~ purrr::map(.x, unlist))
        # convert named-list columns to tibbles
        d <- d %>%
          dplyr::mutate_at(.vars = dplyr::vars(dplyr::one_of('run_args', 'run_environment')),
                           .funs = ~ purrr::map(.x, ~ tibble::as_tibble(purrr::compact(.x))))
        # convert run_started_at into date-time field
        if ('run_started_on' %in% names(d)) {
          d <- d %>%
            dplyr::mutate(run_start_datetime = lubridate::ymd_hms(.data$run_started_on))
        }
      })
  } else {
    d <- tibble::tibble(run_id = character(0))
    futile.logger::flog.info('No runs returned.')
  }
  d
}


