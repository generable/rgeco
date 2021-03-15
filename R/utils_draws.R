#' @importFrom reticulate py_install
setup_reticulate <- function() {
  library(reticulate)
  #reticulate::conda_create('rgeco')
  #reticulate::use_condaenv('rgeco', required=TRUE)
  reticulate::py_install(packages = c('xarray', 'netCDF4'))
}

convert_draws_to_df <- function(resp) {
  setup_reticulate()
  xr <- reticulate::import('xarray', convert = FALSE)
  py_dict <- reticulate::r_to_py(resp$content)
  py_dataset <- xr$Dataset$from_dict(py_dict)
  py_df <- py_dataset$to_dataframe()$reset_index()
  df <- reticulate::py_to_r(py_df) %>%
    dplyr::rename(.chain = .data$chain,
                  .iteration = .data$draw) %>%
    dplyr::mutate(.chain = .data$.chain + 1,
                  .iteration = .data$.iteration + 1,
                  .draw = dplyr::dense_rank(stringr::str_c(.data$.chain, sprintf(.data$.iteration, fmt = '%04.0f'), sep = ':')))
}

