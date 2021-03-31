
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
fetch_inference_dataset_info <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  datasets <- geco_api(IDATA, project_version_id = pv_id)
  if (httr::http_error(datasets$response)) {
    stop(stringr::str_c('Error querying API: ', datasets$response))
  }
  # base dataset response
  dataset_no_pars <- datasets$content %>%
    purrr::map(purrr::list_modify, params = NULL)
  d <- as_dataframe.geco_api_data(content = dataset_no_pars, flatten_names = NULL)

  # handle params
  dataset_params <- datasets$content %>%
    purrr::map('params') %>%
    purrr::set_names(purrr::map_chr(dataset_no_pars, 'run_id'))
  all_param_names <- dataset_params %>% purrr::map(names) %>% unlist() %>% unique()
  if ('inputs' %in% all_param_names) {
    d_inputs <- dataset_params %>%
      purrr::map('inputs') %>%
      purrr::map(unlist, recursive = F) %>%
      purrr::map_dfr(~ tibble::tibble(!!!.x), .id = 'run_id') %>%
      dplyr::rename_at(.vars = dplyr::vars(-.data$run_id), .funs = ~ stringr::str_c('args_',.x))
    d <- dplyr::left_join(d, d_inputs, by = 'run_id')
  }
  if ('sampling_scheme' %in% all_param_names) {
    d_sampling <- dataset_params %>%
      purrr::map('sampling_scheme') %>%
      purrr::map(unlist, recursive = F) %>%
      purrr::map_dfr(~ tibble::tibble(!!!.x), .id = 'run_id') %>%
      dplyr::rename_at(.vars = dplyr::vars(-.data$run_id), .funs = ~ stringr::str_c('args_sampling_',.x))
    d <- dplyr::left_join(d, d_sampling, by = 'run_id')
  }
  if ('seed_subjects' %in% all_param_names) {
    d_subject_seed <- dataset_params %>%
      purrr::map('seed_subjects') %>%
      purrr::map_dfr(~ tibble::tibble(seed_subjects = .x), .id = 'run_id') %>%
      dplyr::rename_at(.vars = dplyr::vars(-.data$run_id), .funs = ~ stringr::str_c('args_sampling_',.x))
    d <- dplyr::left_join(d, d_subject_seed, by = 'run_id')
  }
  if ('seed_truncation' %in% all_param_names) {
    d_trunc_seed <- dataset_params %>%
      purrr::map('seed_truncation') %>%
      purrr::map_dfr(~ tibble::tibble(seed_truncation = .x), .id = 'run_id') %>%
      dplyr::rename_at(.vars = dplyr::vars(-.data$run_id), .funs = ~ stringr::str_c('args_sampling_',.x))
    d <- dplyr::left_join(d, d_trunc_seed, by = 'run_id')
  }
  suppressWarnings({
    d <- d %>%
      dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of(c('run_id', 'project_id', 'project_version_id'))),
                       .funs = ~ stringr::str_c('dataset_', .x))
  })
  d
}



#' @importFrom magrittr %>%
#' @importFrom rlang !!!
fetch_inference_data <- function(run_id, project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  resp <- geco_api(IRUNDATA, project_version_id = pv_id, run_id=run_id)
  d <- .format_inference_data(resp$content)
  d
}

.format_result <- function(results, name) {
  if (name == 'biomarkers') {
    .format_data(results, numeric_fields = c('measurement_value'))
  } else if (name == 'subjects') {
    .format_data(results, numeric_fields = c('smoking_pack_years', 'last_evaluable_assessment'))
  } else {
    .format_data(results)
  }
}

.discover_numeric_fields <- function(results) {
  contains_numeric <- results %>% purrr::transpose() %>% purrr::map_depth(2, is.numeric) %>% purrr::map_lgl(~ any(unlist(.x)))
  names(contains_numeric)[contains_numeric]
}

.format_data <- function(results, numeric_fields = c()) {
  numeric_fields <- c(numeric_fields, .discover_numeric_fields(results))
  results <- results %>%
    purrr::map(tibble::as_tibble)
  if (length(numeric_fields) > 0)
    results <- results %>%
    purrr::map(dplyr::mutate_at, .vars = dplyr::vars(dplyr::one_of(numeric_fields)), .funs = as.double)
  dplyr::bind_rows(results)
}

.format_inference_data <- function(content) {
  results <- content %>%
    purrr::map('data') %>%
    purrr::set_names(purrr::map_chr(content, 'table_name')) %>%
    purrr::compact() %>%
    purrr::imap(.f = ~ .format_result(results = .x, name = .y))
}

