
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
fetch_inference_dataset_info <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  datasets <- geco_api(IDATA, project_version_id = pv_id)
  d <- as_dataframe.geco_api_data(datasets, flatten_names = NULL)
  if ('inputs' %in% names(d$params)) {
    d_inputs <- d$params$inputs %>%
      tibble::tibble(!!!.) %>%
      dplyr::rename_all(.funs = ~ stringr::str_c('args_',.x))
    d <- dplyr::bind_cols(d, d_inputs)
  }
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of(c('run_id', 'project_id', 'project_version_id'))),
                        .funs = ~ stringr::str_c('dataset_', .x))
  })
  d
}

