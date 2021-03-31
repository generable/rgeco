.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
}


convert_xarray_to_df <- function(resp, name = NULL) {
  xr <- reticulate::import('xarray', convert = FALSE)
  if (length(resp$content) > 0) {
    py_dict <- reticulate::r_to_py(resp$content)
    py_dataset <- xr$Dataset$from_dict(py_dict)
    py_df <- py_dataset$to_dataframe()$reset_index()
    df <- reticulate::py_to_r(py_df)
    if (!is.null(name)) {
      quo_name = rlang::sym(name)
      df <- df %>%
        tidyr::gather(.data$.variable, .data$.value, !!quo_name)
    }
  } else {
    futile.logger::flog.info('No draws returned.')
    df <- tibble::tibble()
  }
  df
}

convert_draws_to_df <- function(resp, name = NULL) {
  df <- convert_xarray_to_df(resp, name = name)
  if (nrow(df) > 0) {
    df <- df %>%
      dplyr::rename(.chain = .data$chain,
                    .iteration = .data$draw) %>%
      dplyr::mutate(.chain = .data$.chain + 1,
                    .iteration = .data$.iteration + 1,
                    .draw = dplyr::dense_rank(stringr::str_c(.data$.chain, sprintf(.data$.iteration, fmt = '%04.0f'), sep = ':')))
  }
  df
}



.width_from_quantile <- function(quantile) {
  dplyr::case_when(quantile < 0.5 ~ 1 - round(2*quantile, digits = 2),
                   quantile > 0.5 ~ 1 - round(2*(1-quantile), digits = 2),
                   quantile == 0.5 ~ NA_real_)
}

format_quantiles_as_widths <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }
  df %>%
    dplyr::mutate(.width = .width_from_quantile(.data$quantile),
                  .label = dplyr::case_when(.data$quantile < 0.5 ~ '.lower',
                                            .data$quantile > 0.5 ~ '.upper',
                                            .data$quantile == 0.5 ~ '.median')) %>%
    dplyr::select(-.data$quantile) %>%
    tidyr::spread(.data$.label, .data$.value) %>%
    tidyr::fill(.data$.median, .direction = 'updown') %>%
    dplyr::filter(!is.na(.data$.width))
}
