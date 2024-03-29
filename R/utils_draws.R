
convert_xarray_to_df <- function(resp, name = NULL) {
  .install_xarray()
  if (length(resp$content) == 0) {
    futile.logger::flog.info('No draws returned.')
    df <- tibble::tibble()
  } else {
    py_dict <- reticulate::r_to_py(resp$content)
    if (length(py_dict$data_vars) == 0) {
      futile.logger::flog.info('No draws returned.')
      df <- tibble::tibble()
    } else {
      py_dataset <- xarray$Dataset$from_dict(py_dict)
      py_df <- py_dataset$to_dataframe()$reset_index()
      df <- reticulate::py_to_r(py_df)
      # unlist list-columns that are just scalars
      if (!is.null(name)) {
        quo_name = rlang::sym(name)
        df <- df %>%
          tidyr::pivot_longer(c(quo_name), names_to = '.variable', values_to = '.value')
        df <- df %>%
          dplyr::mutate(length_values = purrr::map_int(.data$.value, length)) %>%
          dplyr::filter(.data$length_values == 1) %>%
          dplyr::select(-.data$length_values) %>%
          dplyr::mutate_if(rlang::is_list, unlist)
      }
      if (any(stringr::str_detect(names(df), pattern = '^subject\\.'))) {
        df <- df %>%
          dplyr::select(-dplyr::starts_with('subject.'))
      }
      if (any(stringr::str_detect(names(df), pattern = '^trial_arm\\.[^\\d]+'))) {
        names <- names(df)[stringr::str_detect(names(df), pattern = '^trial_arm\\.[^\\d]+')]
        df <- df %>%
          dplyr::select(-dplyr::one_of(names))
      }
      if ('trial_arm.1' %in% names(df)) {
        df <- df %>%
          dplyr::rename(control_arm_id = .data$`trial_arm.1`)
      }
      if ('trial_arm' %in% names(df)) {
        df <- df %>%
          dplyr::rename(trial_arm_id = .data$trial_arm)
      }
      if ('subject' %in% names(df)) {
        df <- df %>%
          dplyr::rename(subject_id = .data$subject)
      }
      if ('study' %in% names(df)) {
        df <- df %>%
          dplyr::rename(study_id = .data$study)
      }
    }
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

.quantile_from_width <- function(width) {
  quantiles <- c(0.5 - width/2, 0.5 + width/2)
  round(quantiles, digits = 2)
}

#' Format a long summary of parameter quantiles (one record per run, parameter, and quantile) into a wide format (one record per run, parameter, and interval-width)
#'
#' @note
#' This function expects a data.frame in the format returned by \code{\link{fetch_quantiles}}.
#'
#' @param df a data.frame with quantile summary in long or denormalized format
#' @return a data.frame with new fields (compatible with ggdist plotting functions): .width, .lower, .upper, and .value
#' @export
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
    dplyr::filter(!is.na(.data$.width)) %>%
    dplyr::mutate(.point = 'median',
                  .interval = 'qi') %>%
    dplyr::rename(.value = .data$.median)
}


