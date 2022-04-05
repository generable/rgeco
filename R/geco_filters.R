
.apply_single_filter <- function(data, filter_name, filter_values) {
  quo_filter_name <- rlang::sym(filter_name)
  data %>%
    dplyr::filter(!!quo_filter_name %in% filter_values)
}

.apply_filters <- function(data, filter) {
  modified_filter <- .subset_filter_keys(filter, supported_keys = names(data))
  if (length(modified_filter) > 0) {
    data <- purrr::reduce2(.init = data, .f = .apply_single_filter, .x = names(modified_filter), .y = modified_filter)
  }
  data
}

.prepare_filter <- function(filter, endpoint) {
  supported_keys <- .get_supported_keys(endpoint)
  modified_filter <- .subset_filter_keys(filter, supported_keys)
  modified_filter %>%
    .format_filter_for_api('data')
}

.check_format <- function(filter, alert = FALSE) {
  if (is.null(filter) || length(filter) == 0) {
    filter <- list()
  }
  if (rlang::is_bare_character(filter) && all(purrr::map_lgl(filter, ~ purrr::is_list(.x) | purrr::is_scalar_atomic(.x)))) {
    # we can convert this to a list safely
    filter <- rlang::as_list(filter)
    if (alert) {
      cli::cli_alert_warning(glue::glue('`where` was provided as a named vector, but these are not always formatted as expected. It is safer to format using a list.'))
    }
  } else if (rlang::is_bare_character(filter)) {
    stop('contents of where clause must be a named list of lists, but a vector was provided that could not be safely converted.')
  } else if (!rlang::is_list(filter)) {
    stop('contents of where clause must be a named list of lists, but a list was not provided.')
  } else if (!rlang::is_named2(filter)) {
    stop('contents of where clause must be a named list of lists, but names were not provided.')
  }

  # at this point we know we have a named list of something.
  # check contents.
  updated_filter <- filter %>%
    purrr::map_if(rlang::is_vector, rlang::as_list)

  updated_filter
}

.format_filter_for_api <- function(filter, type = 'data') {
  filter <- .check_format(filter)
  filter <- filter %>%
      purrr::map(stringr::str_c, collapse = ',')
}

.subset_filter_keys <- function(user_filter, supported_keys) {
  if (length(user_filter) == 0) {
    return(c())
  } else {
    keys_subset <- intersect(names(user_filter), supported_keys)
    return(user_filter[keys_subset])
  }
}

.get_supported_keys <- function(endpoint) {
  if (endpoint == 'TRIALS') {
    c()
  } else if (endpoint == 'TRIALARMS') {
    c('trial_id', 'trial_arm_id')
  } else if (endpoint == 'SUBJECTS') {
    c('trial_id', 'trial_arm_id', 'age_min', 'age_max')
  } else if (endpoint == 'EVENTS') {
    c('trial_id', 'trial_arm_id', 'subject_id', 'event_type')
  } else if (endpoint == 'TIMEVARYING') {
    c('trial_id', 'trial_arm_id', 'subject_id', 'measurement_name')
  } else if (endpoint == 'DOSE') {
    c('trial_id', 'trial_arm_id', 'subject_id')
  } else if (endpoint == 'LESIONS') {
    c('trial_id', 'trial_arm_id', 'subject_id')
  } else if (endpoint == 'LESIONTV') {
    c('trial_id', 'trial_arm_id', 'subject_id', 'lesion_id', 'measurement_name')
  } else {
    c()
  }
}

.update_filter <- function(filter, ...) {
  updates <- rlang::list2(...)
  updates <- .check_format(updates)
  filter %>%
    purrr::list_merge(!!!updates)
}
