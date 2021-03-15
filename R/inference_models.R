
#' @importFrom magrittr %>%
fetch_inference_models <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  models <- geco_api(IMODELS, project_version_id = pv_id)
  if (length(models$content) > 0) {
    d <- models$content %>%
      purrr::map_dfr(tibble::enframe) %>%
      tidyr::spread(name, value) %>%
      dplyr::select_if(.predicate = ~ all(!is.null(unlist(.x)))) %>%
      tidyr::unnest(cols = c(one_of('descirption', 'has_priors', 'hash', 'id', 'inference_engine', 'name', 'run_id', 'type', 'version')))
  } else {
    d <- tibble::tibble(id = character(0))
  }
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of(c('run_id'))),
                       .funs = ~ stringr::str_c('model_', .x))
  })
  d
}

