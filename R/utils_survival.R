.tidy_km_strata <- function (data)
{
  if ("strata" %in% names(data)) {
    data %>% tidyr::separate(strata, into = stringr::str_c("strata",
                                                           seq_len(100)), sep = ",", remove = TRUE, extra = "drop",
                             fill = "right") %>% dplyr::mutate(row = dplyr::row_number()) %>%
      tidyr::gather(varname, value, dplyr::starts_with("strata")) %>%
      dplyr::filter(!is.na(value)) %>% dplyr::select(-varname) %>%
      tidyr::separate(value, into = c("varname", "value"),
                      sep = "=", extra = "merge") %>% dplyr::mutate(varname = stringr::str_trim(varname),
                                                                    value = stringr::str_trim(value)) %>% tidyr::spread(varname,
                                                                                                                        value) %>% dplyr::select(-row)
  }
  else {
    futile.logger::flog.warn("`strata` not among variable names; returning data as-is.")
    data
  }
}

#' Prepare kaplan-meier estimates in a tidy format
#' @param data dataframe in which to evaluate formula
#' @param formula formula to be passed to `survival::survfit`
#' @return data.frame with kaplan-meier estimates over time
#' @importFrom broom tidy
#' @importFrom survival survfit
#' @export
prep_km_data <- function (data, formula)
{
  km_df <- survival::survfit(formula = formula, data = data) %>%
    broom::tidy()
  if ("strata" %in% names(km_df)) {
    km_df %>% .tidy_km_strata()
  }
  else if (length(as.character(formula)[3]) > 0) {
    rhs_vars <- rlang::syms(as.character(formula)[3])
    vals <- data %>% distinct(!!!rhs_vars) %>% na.omit()
    km_df %>% tidyr::expand_grid(vals)
  }
  else {
    km_df
  }
}
