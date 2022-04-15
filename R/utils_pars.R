
.VALID_PARAMETER_TYPES = c('biomarker', 'survival', 'median_survival', 'hazard', 'biomarker_params', 'association_state', 'hazard_betas', 'hazard_ratio')
.VALID_PARAMETER_LEVELS = c('subject', 'trial_arm', 'study', 'overall')

.get_pars_by_type <- function(type,
                              level,
                              include_noise = NULL,
                              project = NULL, project_version_id = NULL
                              ) {
  checkmate::assert_choice(type, choices = .VALID_PARAMETER_TYPES, null.ok = FALSE)
  checkmate::assert_choice(level, choices = .VALID_PARAMETER_LEVELS, null.ok = FALSE)
  checkmate::assert_logical(include_noise, len = 1, null.ok = TRUE)

  if (type == 'biomarker') {
    return(.get_pars_predicted_biomarker(level = level, include_noise = include_noise))
  }
  if (type == 'biomarker_params') {
    return(.get_pars_biomarker(level = level))
  }
  if (type == 'association_state') {
    return(.get_pars_association_state(level = level))
  }
  if (type == 'survival') {
    return(.get_pars_predicted_survival(level = level))
  }
  if (type == 'median_survival') {
    return(.get_pars_median_survival(level = level))
  }
  if (type == 'hazard') {
    return(.get_pars_predicted_hazard(level = level))
  }
  if (type == 'hazard_betas') {
    return(.get_pars_hazard_betas(level = level))
  }
  stop('Other types not yet implemented.')
}


.format_filters <- function(type,
                            level,
                            project_version_id,
                            filters,
                            return,
                            limits) {
  checkmate::assert_choice(type, choices = .VALID_PARAMETER_TYPES, null.ok = FALSE)
  checkmate::assert_choice(level, choices = .VALID_PARAMETER_LEVELS, null.ok = FALSE)
  # format filters on content
  if (level == 'trial_arm') {
    f = .get_filters_for_trial_arm(filters, project_version_id)
  }
  else if (level == 'subject') {
    f = .get_filters_for_subject(filters, project_version_id)
  }
  else if (level == 'trial') {
    f = .get_filters_for_trial(filters, project_version_id)
  }
  else if (level == 'overall') {
    f = .get_filters_for_overall(filters, project_version_id)
  } else {
    stop('Other levels not yet implemented.')
  }
  # format filters on draws/quantiles/etc
  if (return == 'draws') {
    l = list(draw = limits$draws, chain = limits$chains)
  } else if (return == 'quantiles') {
    l = list(quantile = limits$quantiles)
  } else if (return == 'median') {
    l = list(quantile = 0.5)
  } else if (return == 'intervals') {
    if (!all(limits$interval %in% c(0.5, 0.8, 0.9))) {
      futile.logger::flog.warn('Only 50, 80, 90% intervals are currently supported. Others will be dropped.')
    }
    quantiles = purrr::map(limits$interval, .quantile_from_width) %>% unlist()
    l = list(quantile = c(quantiles, 0.5))
  }
  purrr::list_modify(f, !!!l) %>% purrr::compact()
}

.get_filters_for_trial_arm <- function(filters, project_version_id) {
  filters <- purrr::compact(filters)
  s <- fetch_subjects(project_version_id=project_version_id, .dots = filters)
  return(list(trial_arm = unique(s$trial_arm_id)))
}

.get_filters_for_subject <- function(filters, project_version_id) {
  filters <- purrr::compact(filters)
  s <- fetch_subjects(project_version_id=project_version_id, .dots = filters)
  return(list(subject = unique(s$subject_id)))
}

.get_filters_for_trial <- function(filters, project_version_id) {
  filters <- purrr::compact(filters)
  s <- fetch_subjects(project_version_id=project_version_id, .dots = filters)
  return(list(trial = unique(s$trial_id)))
}

.get_filters_for_overall <- function(filters, project_version_id) {
  return(list())
}

.get_pars_predicted_biomarker <- function(level, include_noise) {
  if (is.null(include_noise)) {
    stop('include_noise cannot be NULL.')
  }
  if (level == 'subject') {
    if (isTRUE(include_noise)) {
      return(list(predicted_biomarker = list(par = 'predicted_biomarker', trans = NULL, level = 'subject')))
    } else {
      return(list(predicted_biomarker = list(par = 'predicted_biomarker', trans = NULL, level = 'subject')))
    }
  }
  if (level == 'trial_arm') {
    if (isTRUE(include_noise)) {
      return(list(predicted_biomarker = list(par = 'predicted_biomarker_per_trial_arm', trans = NULL, level = 'trial_arm')))
    } else {
      return(list(predicted_biomarker = list(par = 'predicted_biomarker_hat_per_trial_arm', trans = NULL, level = 'trial_arm')))
    }
  }
  if (level == 'overall') {
    if (isTRUE(include_noise)) {
      return(list(predicted_biomarker = list(par = 'predicted_biomarker_overall', trans = NULL, level = 'overall')))
    } else {
      return(list(predicted_biomarker = list(par = 'predicted_biomarker_hat_overall', trans = NULL, level = 'overall')))
    }
  }
  stop(glue::glue('Predicted biomarker values are not available at the {level} level.'))
}

#' @importFrom boot inv.logit
.get_pars_biomarker <- function(level, include_noise = NULL) {
  if (!is.null(include_noise)) {
    stop('include_noise must be NULL.')
  }
  if (level == 'subject') {
      return(
        list(par = list('kg', 'ks', 'f'), trans = list(NULL, NULL, NULL), level = list('subject', 'subject', 'subject')) %>%
          purrr::transpose() %>%
          purrr::set_names(c('kg', 'ks', 'f'))
      )
  }
  if (level == 'trial_arm') {
    return(
      list(par = list('log_kg_trial_arm', 'log_ks_trial_arm', 'logit_f_trial_arm'),
           trans = list(exp, exp, boot::inv.logit),
           level = list('trial_arm', 'trial_arm', 'trial_arm')) %>%
        purrr::transpose() %>%
        purrr::set_names(c('kg', 'ks', 'f'))
    )
  }
  if (level == 'overall') {
    return(
      list(par = list('log_kg_overall', 'log_ks_overall', 'logit_f_overall'),
           trans = list(exp, exp, boot::inv.logit),
           level = list('overall', 'overall', 'overall')) %>%
        purrr::transpose() %>%
        purrr::set_names(c('kg', 'ks', 'f'))
    )
  }
  stop(glue::glue('Biomarker params are not available at the {level} level.'))
}

.get_pars_association_state <- function(level) {
  if (level == 'subject') {
    return(
      list(par = list('association_states'), trans = list(NULL), level = 'subject') %>%
        purrr::transpose() %>%
        purrr::set_names(c('association_state'))
    )
  }
  if (level == 'trial_arm') {
    return(
      list(par = list('predicted_trial_arm_state'), trans = list(NULL), level = 'trial_arm') %>%
        purrr::transpose() %>%
        purrr::set_names(c('association_state'))
    )
  }
  stop(glue::glue('Association state is not available at the {level} level.'))
}

.get_pars_predicted_survival <- function(level) {
  if (level == 'subject') {
    return(list(predicted_survival = list(par = 'predicted_survival', trans = NULL, level = level)))
  }
  if (level == 'trial_arm') {
    return(list(predicted_survival = list(par = 'predicted_survival_per_trial_arm', trans = NULL, level = level)))
  }
  if (level == 'study') {
    return(list(predicted_survival = list(par = 'predicted_survival_per_study', trans = NULL, level = level)))
  }
  if (level == 'overall') {
    return(list(predicted_survival = list(par = 'predicted_survival_overall', trans = NULL, level = level)))
  }
  stop(glue::glue('Predicted survival is not available at the {level} level.'))
}

.get_pars_median_survival <- function(level) {
  if (level == 'subject') {
    return(list(predicted_median_survival = list(par = 'predicted_median_survival', trans = NULL, level = level)))
  }
  if (level == 'trial_arm') {
    return(list(predicted_median_survival = list(par = 'predicted_median_survival_per_trial_arm', trans = NULL, level = level)))
  }
  if (level == 'study') {
    return(list(predicted_median_survival = list(par = 'predicted_median_survival_per_study', trans = NULL, level = level)))
  }
  if (level == 'overall') {
    return(list(predicted_median_survival = list(par = 'predicted_median_survival_overall', trans = NULL, level = level)))
  }
  stop(glue::glue('Predicted median survival is not available at the {level} level.'))
}

.get_pars_predicted_hazard <- function(level) {
  if (level == 'subject') {
    return(list(predicted_hazard = list(par = 'predicted_hazard', trans = NULL, level = level)))
  }
  if (level == 'trial_arm') {
    return(list(predicted_hazard = list(par = 'predicted_hazard_per_trial_arm', trans = NULL, level = level)))
  }
  if (level == 'study') {
    return(list(predicted_hazard = list(par = 'predicted_hazard_per_study', trans = NULL, level = level)))
  }
  if (level == 'overall') {
    return(list(predicted_hazard = list(par = 'predicted_hazard_overall', trans = NULL, level = level)))
  }
  stop(glue::glue('Predicted hazard is not available at the {level} level.'))
}

.get_pars_hazard_betas <- function(level) {
  if (level == 'overall') {
    return(list(smoking_exposure_betas = list(par = 'smoking_exposure_betas', trans = NULL, level = level),
                association_betas = list(par = 'association_betas', trans = NULL, level = level)))
  }
  stop(glue::glue('Hazard betas are not available at the {level} level.'))
}


