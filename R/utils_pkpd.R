

#' Query Generable API for pkpd data in a standard format (biomarkers data and dosing data, merged)
#' returns a data.frame suitable for plotting and analysis.
#' @param project (chr) Name of project to return data for
#' @param project_version_id (chr) Optionally, a specific version of project data to return, if not the most recent
#' @param pk_measure measurement_name of PK measurement (defaults to 'conc', NULL indicates no PK marker)
#' @param pd_measure measurement_name of PD measurement (defaults to NULL - no PD marker)
#' @return data.frame containing merged biomarker & dose data for the PK & PD parameter selected, with columns annotating cycles, time since last SDA, and measurement type.
#' @export
fetch_pkpd <- function(project = NULL, project_version_id = NULL, pd_measure = NULL, pk_measure = 'concentration') {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  futile.logger::flog.info('Querying API for biomarkers data ...')
  b <- fetch_biomarkers(project_version_id = pv_id, measurement_name = purrr::compact(c(pd_measure, pk_measure)), annotate = T, annotate_doses = F)
  futile.logger::flog.info('Querying API for dosing data ...')
  d <- fetch_doses(project_version_id = pv_id)
  futile.logger::flog.info('Merging biomarkers and dosing data, adding annotated fields')
  pkpd <- prep_pkpd_data(biomarkers_data = b, dose_data = d, pd_measure = pd_measure, pk_measure = pk_measure)
}

#' Merge and annotate pkpd biomarkers data with dosing data
#' returns a data.frame suitable for plotting and analysis.
#' @param biomarkers_data data.frame containing biomarkers data
#' @param dose_data data.frame containing dose data
#' @param pk_measure measurement_name of PK measurement (defaults to 'conc', NULL indicates no PK marker)
#' @param pd_measure measurement_name of PD measurement (defaults to NULL - no PD marker)
#' @return data.frame containing merged biomarker & dose data for the PK & PD parameter selected, with columns annotating cycles, time since last SDA, and measurement type.
#' @export
prep_pkpd_data <- function(biomarkers_data, dose_data, pd_measure = NULL, pk_measure = 'concentration') {
  if (nrow(dose_data) == 0) {
    futile.logger::flog.warn('No records in dose_data.')
    return(annotate_pkpd_data(biomarkers_data, pd_measure = pd_measure, pk_measure = pk_measure))
  }
  if (!is.null(pk_measure) & !(pk_measure %in% unique(biomarkers_data$measurement_name))) {
    futile.logger::flog.warn(glue::glue('pk_measure ({pk_measure}) not among the measurements in biomarkers_data ({glue::glue_collapse(unique(biomarkers_data$measurement_name), sep = ", ", last = ", and ")}).'))
  }
  if (!is.null(pd_measure) & !(pk_measure %in% unique(biomarkers_data$measurement_name))) {
    futile.logger::flog.warn(glue::glue('pd_measure ({pd_measure}) not among the measurements in biomarkers_data ({glue::glue_collapse(unique(biomarkers_data$measurement_name), sep = ", ", last = ", and ")}).'))
  }
  if (!'start_hours' %in% names(dose_data)) {
    stop('dose_data does not have start_hours data. Cannot prepare pkpd data without a formatted start time.')
  }
  dose_data_renamed <- dose_data %>%
    dplyr::rename_at(.vars = dplyr::vars(-.data$subject_id, -.data$drug), .funs = ~ stringr::str_c('dose_', .x)) %>%
    dplyr::mutate(hours = .data$dose_start_hours)
  if ('collection_timepoint' %in% names(biomarkers_data)) {
    merged_data <- biomarkers_data %>%
      dplyr::mutate(.dir = dplyr::if_else(.data$collection_timepoint == 'Pre-infusion', 'forward', 'reverse')) %>%
      rolling_join(dose_data_renamed,
                   by = 'subject_id',
                   on = 'hours',
                   direction_field = '.dir',
                   how = 'left',
                   suffix = c('', '.dose')) %>%
      dplyr::select(-.data$hours.dose, -.data$.dir)
  } else {
    merged_data <- rolling_join(biomarkers_data,
                                dose_data_renamed,
                                by = 'subject_id',
                                on = 'hours',
                                direction = 'reverse',
                                how = 'left',
                                suffix = c('', '.dose')) %>%
      dplyr::select(-.data$hours.dose)
  }
  if (nrow(merged_data) != nrow(biomarkers_data)) {
    futile.logger::flog.warn(glue::glue("Number of records in biomarkers data changed after join, from {nrow(biomarkers_data)} to {nrow(merged_data)}."))
  }
  pkpd_data <- annotate_pkpd_data(merged_data, pd_measure = pd_measure, pk_measure = pk_measure)
  if (nrow(pkpd_data) != nrow(biomarkers_data)) {
    futile.logger::flog.warn(glue::glue("Number of records in biomarkers data changed after annotation, from {nrow(biomarkers_data)} to {nrow(pkpd_data)}."))
  }
  pkpd_data
}

#' @importFrom rlang !!
#' @importFrom rlang :=
rolling_join <- function(a, b, by, on, how = c('left', 'inner'),
                         direction = c('forward', 'reverse'),
                         direction_field = NULL,
                         suffix = c('.a', '.b')) {
  if (!is.null(direction_field)) {
    checkmate::assert_character(direction_field, len = 1)
    direction <- rlang::ensym(direction_field)
  } else {
    direction <- match.arg(direction, several.ok = F)
  }
  checkmate::assert_character(suffix, len = 2)
  checkmate::assert_character(by, min.len = 1)
  checkmate::assert_character(on, len = 1)
  by_syms <- rlang::ensyms(by)
  on_sym <- rlang::ensyms(on)
  suffix_a <- suffix[1]
  suffix_b <- suffix[2]
  # filter datasets according to `how` logic
  how <- match.arg(how, several.ok = F)
  if (how %in% c('left', 'inner')) {
    b <- b %>%
      dplyr::semi_join(a, by = by)
  } else if (how %in% c('inner')) {
    a <- a %>%
      dplyr::semi_join(b, by = by)
  }
  # rename `on` vars to be distinct per source (to: `{varname}.{source}`)
  a <- a %>%
    # create a unique identifier for `a` records
    dplyr::mutate(`.id.a` = dplyr::row_number()) %>%
    dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(on)),
                     .funs = ~ stringr::str_c(.x, suffix_a))
  b <- b %>%
    dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(on)),
                     .funs = ~ stringr::str_c(.x, suffix_b))
  id_sym_a <- rlang::sym('.id.a')
  on_sym_a <- rlang::sym(glue::glue('{on}{suffix_a}'))
  on_sym_b <- rlang::sym(glue::glue('{on}{suffix_b}'))
  # do a full cross join, keeping first obs from b after each obs from a
  merged <- dplyr::full_join(a, b, by = by, suffix = suffix) %>%
    dplyr::group_by(!!id_sym_a) %>%
    dplyr::mutate(`.on.diff` = dplyr::case_when(!!direction == 'forward' ~ !!on_sym_b - !!on_sym_a,
                                                !!direction == 'reverse' ~ !!on_sym_a - !!on_sym_b,
                                                TRUE ~ NA_real_)) %>%
    dplyr::filter(.data$`.on.diff` >= 0) %>%
    dplyr::mutate(`.on.rank` = dplyr::dense_rank(.data$`.on.diff`)) %>%
    dplyr::filter(.data$`.on.rank` == 1) %>%
    dplyr::ungroup() %>%
    # rename `on.a` to `on`
    dplyr::rename(!!on := !!on_sym_a) %>%
    dplyr::select(-dplyr::starts_with('.on.'))
  # add in records from `a` with no results in `merged` table
  if (how == 'left') {
    merged <- dplyr::bind_rows(merged,
                               a %>% dplyr::anti_join(merged, by = rlang::as_label(id_sym_a)))
  }
  # reorder & clean up results
  merged <- merged %>%
    # sort in original order
    dplyr::arrange(!!id_sym_a) %>%
    dplyr::select(-!!id_sym_a)
  # return result
  merged
}

annotate_pkpd_data <- function(.d, pd_measure = NULL, pk_measure = NULL) {
  # filter to provided biomarkers
  biomarker_names <- c(pd_measure, pk_measure) %>%
    purrr::compact() %>%
    stringr::str_to_lower()
  .d <- .d %>%
    dplyr::mutate(measurement_type = NA_character_)
  # add .type of measurement (pk or pd)
  if (!is.null(pk_measure)) {
    .d <- .d %>%
      dplyr::mutate(measurement_type = dplyr::if_else(.data$measurement_name == pk_measure, 'pk', .data$measurement_type))
  }
  if (!is.null(pd_measure)) {
    .d <- .d %>%
      dplyr::mutate(measurement_type = dplyr::if_else(.data$measurement_name == pd_measure, 'pd', .data$measurement_type))
  }
  # add time to next SDA for observations that are pre-infusion for cycle 1
  if ('dose_start_hours' %in% names(.d)) {
    .d <- .d %>%
      dplyr::group_by(.data$subject_id) %>%
      dplyr::arrange(.data$hours) %>%
      tidyr::fill(dplyr::starts_with('dose'), .direction = 'up') %>%
      dplyr::ungroup()
  }
  # add time since last SDA
  if ('dose_start_hours' %in% names(.d)) {
    .d <- .d %>%
      dplyr::mutate(hours_since_SDA = dplyr::case_when(is.na(dose_start_hours) ~ NA_real_,
                                                       TRUE ~ hours - dose_start_hours))
  }
  # modify collection_timepoint to be an ordered factor
  if ('collection_timepoint' %in% names(.d)) {
    # add time to next SDA
    if ('hours_since_SDA' %in% names(.d)) {
      .d <- .d %>%
        dplyr::mutate(collection_timepoint = factor(.data$collection_timepoint, exclude = c(NA, 'NA')),
                      collection_timepoint = forcats::fct_reorder(.data$collection_timepoint, .data$hours_since_SDA, .fun = min, na.rm = T))
    } else if ('hours' %in% names(.d)) {
      .d <- .d %>%
        dplyr::mutate(collection_timepoint = factor(.data$collection_timepoint, exclude = c(NA, 'NA')),
                      collection_timepoint = forcats::fct_reorder(.data$collection_timepoint, .data$hours, .fun = min, na.rm = T))
    }
  }
  # return .d
  .d
}


