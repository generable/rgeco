
#' @importFrom magrittr %>%
fetch_inference_models <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  models <- geco_api(IMODELS, project_version_id = pv_id)
  if (length(models$content) > 0) {
    d <- models$content %>%
      purrr::map_dfr(tibble::enframe, .id = '.id') %>%
      tidyr::spread(.data$name, .data$value) %>%
      dplyr::select_if(.predicate = ~ all(!is.null(unlist(.x)))) %>%
      tidyr::unnest(cols = c(dplyr::one_of('descirption', 'has_priors', 'hash', 'id', 'inference_engine', 'name', 'run_id', 'type', 'version'))) %>%
      dplyr::select(-.data$.id)
      suppressWarnings({
        d <- d %>%
          dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of(c('run_id'))),
                           .funs = ~ stringr::str_c('model_', .x))
        d <- d %>%
          dplyr::mutate_at(.vars = dplyr::vars(dplyr::one_of('model_parameters', 'model_has_predicted_values')),
                           .funs = ~ purrr::map(.x, unlist))
      })
  } else {
    futile.logger::flog.info('No models returned.')
    d <- tibble::tibble(model_id = character(0))
  }
  d
}

#' @importFrom magrittr %>%
fetch_inference_model_pars <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  models <- geco_api(IPARS, project_version_id = pv_id)
  if (length(models$content) > 0) {
    d <- models$content %>%
      purrr::map_dfr(tibble::enframe, .id = '.id') %>%
      tidyr::spread(.data$name, .data$value) %>%
      dplyr::select_if(.predicate = ~ all(!is.null(unlist(.x))))
    suppressWarnings({
      #d <- d %>%
      #  dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of(c('run_id'))),
      #                   .funs = ~ stringr::str_c('model_', .x))
      d <- d %>%
        dplyr::mutate_at(.vars = dplyr::vars(dplyr::one_of('description', 'model_id')),
                         .funs = ~ unlist(purrr::map(.x, unlist)))

      d <- d %>%
        dplyr::mutate(params = purrr::map(.data$params,
                                          ~ purrr::set_names(.x, purrr::map_chr(.x, 'name')) %>%
                                            purrr::map('dimensions') %>%
                                            purrr::map_dfr(~ tibble::tibble(dim = unlist(.x)), .id = 'parameter'))) %>%
        dplyr::select(-.data$.id, -.data$description) %>%
        dplyr::distinct() %>%
        tidyr::unnest(.data$params)
    })
  } else {
    futile.logger::flog.info('No models returned.')
    d <- tibble::tibble(model_id = character(0))
  }
  d
}


