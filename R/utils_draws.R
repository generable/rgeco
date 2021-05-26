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
      df <- df  %>%
        dplyr::rename_at(.vars = dplyr::vars(dplyr::starts_with('subject.')),
                         .funs = ~ stringr::str_remove(.x, 'subject.'))
    }
    if ('subject' %in% names(df)) {
      df <- df %>%
        dplyr::rename(subject_id = .data$subject)
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

#' Format a long summary of parameter quantiles (one record per run, parameter, and quantile) into a wide format (one record per run, parameter, and interval-width)
#' @param df a data.frame with quantile summary in long or denormalized format
#' @return a data.frame with new fields (compatible with ggdist plotting functions): .width, .lower, .upper, and .median
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
    dplyr::filter(!is.na(.data$.width))
}

.get_default_run <- function(parameter, project = NULL, project_version_id = NULL,
                             type = c('posterior', 'prior'), predictive = F, quantiles = T) {
  type <- match.arg(type, several.ok = F)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  # get name of run_info field containing relevant parameter names
  if (isTRUE(predictive))
    run_info_field <- glue::glue('run_{type}_predictive')
  else if (type == 'prior')
    run_info_field <- 'run_priors'
  else if (type == 'posterior')
    run_info_field <- 'run_parameters'
  else
    stop("You found an error.")
  param_name <- glue::glue('{dplyr::if_else(quantiles, "summarized_", "")}{parameter}')
  futile.logger::flog.debug(glue::glue("looking in {run_info_field} for {param_name}"))
  run_info_sym <- rlang::sym(run_info_field)
  run_id <- list_runs(project_version_id = pv_id) %>%
    dplyr::filter(purrr::map_lgl(!!run_info_sym, ~ param_name %in% .x)) %>%
    dplyr::filter(.data$run_start_datetime == max(.data$run_start_datetime)) %>%
    dplyr::pull(.data$run_id)
  if (length(run_id) > 0)
    futile.logger::flog.info(glue::glue('Fetching results for run: {run_id}.'))
  run_id
}
