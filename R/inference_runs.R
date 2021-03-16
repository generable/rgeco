
#' @importFrom magrittr %>%
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
  } else {
    d <- tibble::tibble(id = character(0))
  }
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of(c('dataset_id', 'model_id', 'run_args'))),
                       .funs = ~ stringr::str_c('run_', .x))
  })
  d
}


