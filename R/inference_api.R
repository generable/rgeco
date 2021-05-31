
.VALID_RETURN_TYPES <- c('median', 'intervals', 'quantiles', 'draws')

fetch_predicted_biomarkers <- function(run_id,
                                       level = c('subject', 'trialarm', 'overall'),
                                       include_noise = c(FALSE, TRUE),
                                       return = c('median', 'quantiles', 'intervals', 'draws'),
                                       type = c('posterior', 'prior'),
                                       project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  include_noise <- match.arg(include_noise)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'biomarkers', level = level, include_noise = include_noise, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id)
}

fetch_predicted_survival <- function(run_id,
                                       level = c('subject', 'trialarm', 'study', 'overall'),
                                       return = c('median', 'quantiles', 'intervals', 'draws'),
                                       type = c('posterior', 'prior'),
                                       project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'survival', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id)
}

fetch_predicted_median_survival <- function(run_id,
                                     level = c('subject', 'trialarm', 'study', 'overall'),
                                     return = c('median', 'quantiles', 'intervals', 'draws'),
                                     type = c('posterior', 'prior'),
                                     project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'median_survival', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id)
}

fetch_predicted_hazard <- function(run_id,
                                   level = c('subject', 'trialarm', 'study', 'overall'),
                                   return = c('median', 'quantiles', 'intervals', 'draws'),
                                   type = c('posterior', 'prior'),
                                   project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'hazard', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id)
}

fetch_biomarker_params <- function(run_id,
                                   level = c('subject', 'trialarm', 'overall'),
                                   return = c('median', 'quantiles', 'intervals', 'draws'),
                                   type = c('posterior', 'prior'),
                                   project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'biomarker_params', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id)
}

fetch_association_state <- function(run_id,
                                   level = c('subject', 'trialarm'),
                                   return = c('median', 'quantiles', 'intervals', 'draws'),
                                   type = c('posterior', 'prior'),
                                   project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'association_state', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id, level = level)
}


.fetch_pars_by_type <- function(parlist, return, run_id, type = c('posterior', 'prior'), level = NULL, project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  checkmate::check_choice(return, choices = .VALID_RETURN_TYPES)
  type = match.arg(type, several.ok = FALSE)

  # get function type according to return value
  if (return == 'median') {
    fun <- .fetch_par_median
    label <- 'median estimates'
  } else if (return == 'quantiles') {
    fun <- .fetch_par_quantiles
    label <- 'quantile estimates'
  } else if (return == 'intervals') {
    fun <- .fetch_par_intervals
    label <- "credible intervals"
  } else if (return == 'draws') {
    fun <- .fetch_par_draws
    label <- 'draws'
  } else {
    stop(glue::glue('return type not defined: {return}'))
  }
  futile.logger::flog.info(glue::glue('Fetching {type} {label} of {glue::glue_collapse(names(parlist), sep = ", ", last = ", and ")} at the {level} level from {length(run_id)} model runs.'))

  fetchfun <- purrr::lift_dl(fun, type = type, run_id = run_id, project_version_id = pv_id)
  parlist %>%
    purrr::map(fetchfun) %>%
    purrr::imap_dfr(~ dplyr::mutate(.x, .variable = .y))
}

.fetch_par_base <- function(par, trans, run_id, project_version_id, type, fetch_fun) {
  d <- fetch_fun(parameter = par, project_version_id = project_version_id, type = type, run_id = run_id, quiet = TRUE)
  if (!is.null(trans)) {
    d <- d %>%
      dplyr::mutate(.value = trans(.value))
  }
  d
}

.fetch_par_quantiles <- function(par, trans, run_id, project_version_id, type) {
  .fetch_par_base(par = par, trans = trans, run_id = run_id, project_version_id = project_version_id, type = type,
                  fetch_fun = fetch_quantiles)
}

.fetch_par_intervals <- function(par, trans, run_id, project_version_id, type) {
  .fetch_par_quantiles(par = par, trans = trans, run_id = run_id, project_version_id = project_version_id, type = type) %>%
    format_quantiles_as_widths() %>%
    dplyr::rename(.median = .data$.value)
}

.fetch_par_median <- function(par, trans, run_id, project_version_id, type) {
  .fetch_par_quantiles(par = par, trans = trans, run_id = run_id, project_version_id = project_version_id, type = type) %>%
    dplyr::filter(.data$quantile == 0.5) %>%
    dplyr::select(-.data$quantile) %>%
    dplyr::rename(.median = .data$.value)
}

.fetch_par_draws <- function(par, trans, run_id, project_version_id, type) {
  .fetch_par_base(par = par, trans = trans, run_id = run_id, project_version_id = project_version_id, type = type,
                  fetch_fun = fetch_draws)
}
