
.VALID_RETURN_TYPES <- c('median', 'intervals', 'quantiles', 'draws')

#' Fetch predicted biomarker values (SLD)
#'
#' Fetch predicted biomarker values at predefined time intervals during the follow-up window.
#'
#' Predicted biomarker values can be provided at different levels of the hierarchical model:
#' \enumerate{
#'   \item Per subject
#'   \item Per trial_arm
#'   \item Overall
#' }
#'
#' Use the `level` argument to select the desired level at which to summarize predicted values.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' @note
#' The columns returned depends on the value of the `return` argument. The default is to return median values for each \code{level} and prediction time (\code{biomarker_time}, in days).
#'
#' All return formats share a set of columns containing meta-information about the predicted quantities:
#' \enumerate{
#'   \item \code{.variable} Text label for the predicted quantity or variable. In this case, "predicted_biomarker"
#'   \item \code{run_id} Text field containing the run_id from which each returned value was generated.
#'   \item \code{.type} Text field containing the type (prior or posterior) of predicted quantity or inferences summarized.
#'   \item \code{.level} Text field containing the level (subject, trial_arm, or overall) at which the predicted values were prepared.
#'   \item \code{biomarker_time} Numeric field containing the study time (days) at which the predicted value was prepared.
#'   \item \code{subject_id | trial_arm_id} If returning predicted values at the subject or trial-arm level, the ids corresponding to the subjects or trial-arms used in the prediction.
#' }
#'
#' In addition, there will be a few columns to provide and describe the predicted quantities.
#' The set of columns included here will depend on the `return` argument:
#' \enumerate{
#'   \item if \code{return  == 'median'}, a pair of columns: \code{.value} and \code{.point}
#'   \item if \code{return == 'quantiles'}, a pair of columns: \code{.value} and \code{quantile}
#'   \item if \code{return == 'intervals'}, a set of columns: \code{.value}, \code{.width}, \code{.lower}, and \code{.upper} containing the median predicted biomarker estimate (.value) along with the lower (.lower) and upper (.upper) bounds for the 50, 80, and 90 percent credible intervals (.width).
#'      \itemize{
#'        \item In addition, columns \code{.point} and \code{.interval} describe the type of point estimate ('median') and interval ('qi')
#'        \item This data structure mimics that returned by \code{\link[tidybayes:median_qi]{median_qi}} function in the \code{\link[tidybayes:tidybayes-package]{tidybayes}} package.
#'        }
#'   \item if \code{return == 'draws'}, a set of columns: \code{.value}, \code{.chain}, \code{.iteration}, and \code{.draw} describing the predicted quantities for each draw, chain and iteration. This data structure mimics the \code{\link[posterior:draws_df]{draws_df}} format from the \code{\link[posterior:posterior-package]{posterior}} package.
#' }
#'
#' @examples
#' \dontrun{
#' library(tidybayes)
#' library(tidyverse)
#'
#' login()
#'
#' # ---- Plot median predicted SLD over time ----
#' d <- fetch_predicted_biomarkers(run_id, level = 'overall')
#'
#' ggplot(d, aes(x = biomarker_time, y = .value)) +
#'   geom_line() +
#'   scale_y_continuous('Predicted SLD', labels = scales::percent)
#'
#'
#' # ---- Plot predicted SLD over time from intervals ----
#' d <- fetch_predicted_biomarkers(run_id, level = 'overall', return = 'intervals')
#'
#' ggplot(d, aes(x = biomarker_time, y = .value, ymin = .lower, ymax = .upper, group = .width)) +
#'   geom_lineribbon(alpha = 0.2) +
#'   scale_y_continuous('Predicted SLD', labels = scales::percent) +
#'   scale_fill_brewer()
#'
#'
#' # ---- Plot predicted SLD by trial-arm over time ----
#' d <- fetch_predicted_biomarkers(run_id, level = 'trial_arm', return = 'intervals') %>%
#'      inner_join(fetch_subjects() %>% distinct(trial_arm_id, trial_arm_name))
#'
#' ggplot(d, aes(x = biomarker_time, y = .value, ymin = .lower, ymax = .upper, group = .width,
#'    fill = trial_arm_name, colour = trial_arm_name)) +
#'   geom_lineribbon(alpha = 0.2) +
#'   scale_y_continuous('Predicted SLD', labels = scales::percent) +
#'   theme(legend.position = 'bottom')
#'
#'
#' # ----Plot predicted SLD over time from draws ----
#' d <- fetch_predicted_biomarkers(run_id, level = 'overall', return = 'draws')
#'
#' ggplot(d, aes(x = biomarker_time, y = .value)) +
#'   stat_lineribbon(alpha = 0.4, .width = c(0.66, 0.99)) +
#'   scale_y_continuous('Predicted SLD', labels = scales::percent) +
#'   scale_fill_brewer() +
#'   theme_minimal()
#'
#' # ---- summarize sampling quality ----
#'
#' library(posterior)
#' d <- fetch_predicted_biomarkers(run_id, level = 'overall', return = 'draws')
#'
#' d %>% spread(.variable, .value) %>%
#'    group_by(.level, biomarker_time, .type, run_id) %>%
#'    group_modify(~ summarise_draws(.x))
#' }
#'
#' @param run_id (str) [required] One or several model run_ids. See \code{\link{find_runs}} for a list of runs available.
#' @param level (str) The level at which to return predicted values. One of: subject, trial_arm, or overall. Default value is per subject.
#' @param include_noise (bool) Whether to include measurement error in the predicted summaries returned. Default: FALSE
#' @param return (str) The type of summary to return. One of: median, quantiles, intervals, or draws. Default: median
#' @param type (str) Whether to return posterior or prior predictions. Default: posterior
#' @param project (str) The name of the project to which the run_id belongs.
#' @param project_version_id (str) The specific project_version_id to which the run_id belongs. Defaults to the most recent project_version_id if none provided.
#'
#' @return data.frame in tidy format, with one record per parameter, run_id, and summarized level. See notes for specific details about each return type.
#' @export
fetch_predicted_biomarkers <- function(run_id,
                                       level = c('subject', 'trial_arm', 'overall'),
                                       include_noise = FALSE,
                                       return = c('median', 'quantiles', 'intervals', 'draws'),
                                       type = c('posterior', 'prior'),
                                       project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  include_noise <- checkmate::assert_logical(include_noise, len = 1, null.ok = FALSE)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'biomarker', level = level, include_noise = include_noise, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id, level = level)
}


#' Fetch predicted survival (\%) over time
#'
#' Fetch predicted survival at predefined time intervals during the follow-up window.
#'
#' Predicted survival values can be provided at different levels of the hierarchical model:
#' \enumerate{
#'   \item Per subject
#'   \item Per trial_arm
#'   \item Per study
#'   \item Overall
#' }
#'
#' Use the `level` argument to select the desired level at which to summarize predicted values.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' @note
#' The columns returned depends on the value of the `return` argument. The default is to return median values for each \code{level} and prediction time (\code{biomarker_time}, in days).
#'
#' All return formats share a set of columns containing meta-information about the predicted quantities:
#' \enumerate{
#'   \item \code{.variable} Text label for the predicted quantity or variable. In this case, "predicted_survival"
#'   \item \code{run_id} Text field containing the run_id from which each returned value was generated.
#'   \item \code{.type} Text field containing the type (prior or posterior) of predicted quantity or inferences summarized.
#'   \item \code{.level} Text field containing the level (subject, trial_arm, or overall) at which the predicted values were prepared.
#'   \item \code{survival_time} Numeric field containing the study time (days) at which the predicted value was prepared.
#'   \item \code{subject_id | trial_arm_id | trial_id } If returning predicted values at the subject, study, or trial-arm level, the ids corresponding to the subjects or trial-arms used in the prediction.
#' }
#'
#' In addition, there will be a few columns to provide and describe the predicted quantities.
#' The set of columns included here will depend on the `return` argument:
#' \enumerate{
#'   \item if \code{return  == 'median'}, a pair of columns: \code{.value} and \code{.point}
#'   \item if \code{return == 'quantiles'}, a pair of columns: \code{.value} and \code{quantile}
#'   \item if \code{return == 'intervals'}, a set of columns: \code{.value}, \code{.width}, \code{.lower}, and \code{.upper} containing the median estimate (.value) along with the lower (.lower) and upper (.upper) bounds for the 50, 80, and 90 percent credible intervals (.width).
#'      \itemize{
#'        \item In addition, columns \code{.point} and \code{.interval} describe the type of point estimate ('median') and interval ('qi')
#'        \item This data structure mimics that returned by \code{\link[tidybayes:median_qi]{median_qi}} function in the \code{\link[tidybayes:tidybayes-package]{tidybayes}} package.
#'        }
#'   \item if \code{return == 'draws'}, a set of columns: \code{.value}, \code{.chain}, \code{.iteration}, and \code{.draw} describing the predicted quantities for each draw, chain and iteration. This data structure mimics the \code{\link[posterior:draws_df]{draws_df}} format from the \code{\link[posterior:posterior-package]{posterior}} package.
#' }
#'
#' @examples
#' \dontrun{
#' library(tidybayes)
#' library(tidyverse)
#'
#' login()
#'
#' # ---- Plot median predicted survival over time ----
#' d <- fetch_predicted_survival(run_id, level = 'overall')
#'
#' ggplot(d, aes(x = survival_time, y = .value)) +
#'   geom_line() +
#'   scale_y_continuous('Predicted Survival', labels = scales::percent)
#'
#'
#' # ---- Plot predicted survival over time from intervals ----
#' d <- fetch_predicted_survival(run_id, level = 'overall', return = 'intervals')
#'
#' ggplot(d, aes(x = survival_time, y = .value, ymin = .lower, ymax = .upper, group = .width)) +
#'   geom_lineribbon(alpha = 0.2) +
#'   scale_y_continuous('Predicted Survival', labels = scales::percent) +
#'   scale_fill_brewer()
#'
#'
#' # ---- Plot predicted survival by trial-arm over time ----
#' d <- fetch_predicted_survival(run_id, level = 'trial_arm', return = 'intervals') %>%
#'      inner_join(fetch_subjects() %>% distinct(trial_arm_id, trial_arm_name))
#'
#' ggplot(d, aes(x = survival_time, y = .value, ymin = .lower, ymax = .upper, group = .width,
#'    fill = trial_arm_name, colour = trial_arm_name)) +
#'   geom_lineribbon(alpha = 0.2) +
#'   scale_y_continuous('Predicted Survival', labels = scales::percent) +
#'   theme(legend.position = 'bottom') +
#'   facet_wrap(~ trial_arm_name)
#'
#'
#' # ----Plot predicted survival over time from draws ----
#' d <- fetch_predicted_survival(run_id, level = 'overall', return = 'draws')
#'
#' ggplot(d, aes(x = survival_time, y = .value)) +
#'   stat_lineribbon(alpha = 0.4, .width = c(0.66, 0.99)) +
#'   scale_y_continuous('Predicted Survival', labels = scales::percent) +
#'   scale_fill_brewer() +
#'   theme_minimal()
#'
#' # ---- summarize sampling quality ----
#'
#' library(posterior)
#' d <- fetch_predicted_survival(run_id, level = 'overall', return = 'draws')
#'
#' d %>% spread(.variable, .value) %>%
#'    group_by(.level, survival_time, .type, run_id) %>%
#'    group_modify(~ summarise_draws(.x))
#' }
#'
#' @param run_id (str) [required] One or several model run_ids. See \code{\link{find_runs}} for a list of runs available.
#' @param level (str) The level at which to return predicted values. One of: subject, trial_arm, study, or overall. Default value is per trial_arm.
#' @param return (str) The type of summary to return. One of: median, quantiles, intervals, or draws. Default: median
#' @param type (str) Whether to return posterior or prior predictions. Default: posterior
#' @param project (str) The name of the project to which the run_id belongs.
#' @param project_version_id (str) The specific project_version_id to which the run_id belongs. Defaults to the most recent project_version_id if none provided.
#'
#' @return data.frame in tidy format, with one record per parameter, run_id, and summarized level. See notes for specific details about each return type.
#' @export
fetch_predicted_survival <- function(run_id,
                                       level = c('trial_arm', 'study', 'subject', 'overall'),
                                       return = c('median', 'quantiles', 'intervals', 'draws'),
                                       type = c('posterior', 'prior'),
                                       project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'survival', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id, level = level)
}

#' Fetch predicted median survival (days)
#'
#' Fetch predicted median survival if observed during the follow-up window.
#'
#' Predicted median survival can be summarized at different levels of the hierarchical model:
#' \enumerate{
#'   \item Per subject
#'   \item Per trial_arm
#'   \item Per study
#'   \item Overall
#' }
#'
#' Use the `level` argument to select the desired level at which to summarize predicted values.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' @note
#' The columns returned depends on the value of the `return` argument. The default is to return median values for each \code{level} and prediction time (\code{biomarker_time}, in days).
#'
#' All return formats share a set of columns containing meta-information about the predicted quantities:
#' \enumerate{
#'   \item \code{.variable} Text label for the predicted quantity or variable. In this case, "predicted_median_survival"
#'   \item \code{run_id} Text field containing the run_id from which each returned value was generated.
#'   \item \code{.type} Text field containing the type (prior or posterior) of predicted quantity or inferences summarized.
#'   \item \code{.level} Text field containing the level (subject, trial_arm, or overall) at which the predicted values were prepared.
#'   \item \code{subject_id | trial_arm_id | trial_id } If returning predicted values at the subject, study, or trial-arm level, the ids corresponding to the subjects or trial-arms used in the prediction.
#' }
#'
#' In addition, there will be a few columns to provide and describe the predicted quantities.
#' The set of columns included here will depend on the `return` argument:
#' \enumerate{
#'   \item if \code{return  == 'median'}, a pair of columns: \code{.value} and \code{.point}
#'   \item if \code{return == 'quantiles'}, a pair of columns: \code{.value} and \code{quantile}
#'   \item if \code{return == 'intervals'}, a set of columns: \code{.value}, \code{.width}, \code{.lower}, and \code{.upper} containing the median estimate (.value) along with the lower (.lower) and upper (.upper) bounds for the 50, 80, and 90 percent credible intervals (.width).
#'      \itemize{
#'        \item In addition, columns \code{.point} and \code{.interval} describe the type of point estimate ('median') and interval ('qi')
#'        \item This data structure mimics that returned by \code{\link[tidybayes:median_qi]{median_qi}} function in the \code{\link[tidybayes:tidybayes-package]{tidybayes}} package.
#'        }
#'   \item if \code{return == 'draws'}, a set of columns: \code{.value}, \code{.chain}, \code{.iteration}, and \code{.draw} describing the predicted quantities for each draw, chain and iteration. This data structure mimics the \code{\link[posterior:draws_df]{draws_df}} format from the \code{\link[posterior:posterior-package]{posterior}} package.
#' }
#'
#' @examples
#' \dontrun{
#' library(tidybayes)
#' library(tidyverse)
#'
#' login()
#'
#' # ---- Plot median predicted median survival by trial_arm ----
#' d <- fetch_predicted_median_survival(run_id, level = 'trial_arm') %>%
#'     left_join(fetch_subjects() %>% distinct(trial_arm_id, trial_arm_name),
#'               by = 'trial_arm_id')
#'
#' ggplot(d, aes(y = trial_arm_name, x = .value)) +
#'   geom_point() +
#'   scale_x_continuous('Predicted Median Survival (days)')
#'
#'
#' # ---- Plot predicted median survival from intervals ----
#' d <- fetch_predicted_median_survival(run_id, level = 'trial_arm', return = 'intervals') %>%
#'     left_join(fetch_subjects() %>% distinct(trial_arm_id, trial_arm_name),
#'               by = 'trial_arm_id')
#'
#' ggplot(d, aes(x = .value, y = trial_arm_name,
#'               xmin = .lower, xmax = .upper, group = .width)) +
#'   geom_pointinterval() +
#'   scale_x_continuous('Predicted Median Survival (days)')
#'
#' # ----Plot predicted median survival from draws ----
#' d <- fetch_predicted_median_survival(run_id, level = 'trial_arm', return = 'draws') %>%
#'     left_join(fetch_subjects() %>% distinct(trial_arm_id, trial_arm_name),
#'               by = 'trial_arm_id')
#'
#' ggplot(d, aes(x = .value, y = trial_arm_name)) +
#'   stat_pointinterval(.width = c(0.66, 0.99)) +
#'   scale_x_continuous('Predicted Median Survival (days)')
#'
#' # ---- summarize Pr[MedianOS > 750] from draws ----
#'
#' d %>%
#'   dplyr::group_by(trial_arm_name) %>%
#'   dplyr::summarize(mean(.value > 750))
#' }
#'
#' @param run_id (str) [required] One or several model run_ids. See \code{\link{find_runs}} for a list of runs available.
#' @param level (str) The level at which to return predicted values. One of: subject, trial_arm, study, or overall. Default value is per trial_arm
#' @param return (str) The type of summary to return. One of: median, quantiles, intervals, or draws. Default: median
#' @param type (str) Whether to return posterior or prior predictions. Default: posterior
#' @param project (str) The name of the project to which the run_id belongs.
#' @param project_version_id (str) The specific project_version_id to which the run_id belongs. Defaults to the most recent project_version_id if none provided.
#'
#' @return data.frame in tidy format, with one record per parameter, run_id, and summarized level. See notes for specific details about each return type.
#' @export
fetch_predicted_median_survival <- function(run_id,
                                     level = c('trial_arm', 'study', 'subject', 'overall'),
                                     return = c('median', 'quantiles', 'intervals', 'draws'),
                                     type = c('posterior', 'prior'),
                                     project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'median_survival', level = level, project_version_id = pv_id)
  d <- .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id, level = level)
}

#' Fetch predicted hazard rate over time
#'
#' Fetch predicted hazard at predefined time intervals during the follow-up window.
#'
#' Predicted hazard values can be provided at different levels of the hierarchical model:
#' \enumerate{
#'   \item Per subject
#'   \item Per trial_arm
#'   \item Per study
#'   \item Overall
#' }
#'
#' Use the `level` argument to select the desired level at which to summarize predicted values.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' @note
#' The columns returned depends on the value of the `return` argument. The default is to return median values for each \code{level} and prediction time (\code{biomarker_time}, in days).
#'
#' All return formats share a set of columns containing meta-information about the predicted quantities:
#' \enumerate{
#'   \item \code{.variable} Text label for the predicted quantity or variable. In this case, "predicted_hazard"
#'   \item \code{run_id} Text field containing the run_id from which each returned value was generated.
#'   \item \code{.type} Text field containing the type (prior or posterior) of predicted quantity or inferences summarized.
#'   \item \code{.level} Text field containing the level (subject, trial_arm, or overall) at which the predicted values were prepared.
#'   \item \code{survival_time} Numeric field containing the study time (days) at which the predicted value was prepared.
#'   \item \code{subject_id | trial_arm_id | trial_id } If returning predicted values at the subject, study, or trial-arm level, the ids corresponding to the subjects or trial-arms used in the prediction.
#' }
#'
#' In addition, there will be a few columns to provide and describe the predicted quantities.
#' The set of columns included here will depend on the `return` argument:
#' \enumerate{
#'   \item if \code{return  == 'median'}, a pair of columns: \code{.value} and \code{.point}
#'   \item if \code{return == 'quantiles'}, a pair of columns: \code{.value} and \code{quantile}
#'   \item if \code{return == 'intervals'}, a set of columns: \code{.value}, \code{.width}, \code{.lower}, and \code{.upper} containing the median estimate (.value) along with the lower (.lower) and upper (.upper) bounds for the 50, 80, and 90 percent credible intervals (.width).
#'      \itemize{
#'        \item In addition, columns \code{.point} and \code{.interval} describe the type of point estimate ('median') and interval ('qi')
#'        \item This data structure mimics that returned by \code{\link[tidybayes:median_qi]{median_qi}} function in the \code{\link[tidybayes:tidybayes-package]{tidybayes}} package.
#'        }
#'   \item if \code{return == 'draws'}, a set of columns: \code{.value}, \code{.chain}, \code{.iteration}, and \code{.draw} describing the predicted quantities for each draw, chain and iteration. This data structure mimics the \code{\link[posterior:draws_df]{draws_df}} format from the \code{\link[posterior:posterior-package]{posterior}} package.
#' }
#'
#' @examples
#' \dontrun{
#' library(tidybayes)
#' library(tidyverse)
#'
#' login()
#'
#' # ---- Plot median predicted hazard over time ----
#' d <- fetch_predicted_hazard(run_id, level = 'overall')
#'
#' ggplot(d, aes(x = survival_time, y = .value)) +
#'   geom_line() +
#'   scale_y_continuous('Predicted Hazard', labels = scales::comma)
#'
#'
#' # ---- Plot predicted hazard over time from intervals ----
#' d <- fetch_predicted_hazard(run_id, level = 'overall', return = 'intervals')
#'
#' ggplot(d, aes(x = survival_time, y = .value, ymin = .lower, ymax = .upper, group = .width)) +
#'   geom_lineribbon(alpha = 0.2) +
#'   scale_y_continuous('Predicted Hazard', labels = scales::comma) +
#'   scale_fill_brewer()
#'
#'
#' # ---- Plot predicted hazard by trial-arm over time ----
#' d <- fetch_predicted_hazard(run_id, level = 'trial_arm', return = 'intervals') %>%
#'      inner_join(fetch_subjects() %>% distinct(trial_arm_id, trial_arm_name))
#'
#' ggplot(d, aes(x = survival_time, y = .value, ymin = .lower, ymax = .upper, group = .width,
#'    fill = trial_arm_name, colour = trial_arm_name)) +
#'   geom_lineribbon(alpha = 0.2) +
#'   scale_y_continuous('Predicted Hazard', labels = scales::comma) +
#'   theme(legend.position = 'bottom') +
#'   facet_wrap(~ trial_arm_name)
#'
#'
#' # ----Plot predicted hazard over time from draws ----
#' d <- fetch_predicted_hazard(run_id, level = 'overall', return = 'draws')
#'
#' ggplot(d, aes(x = survival_time, y = .value)) +
#'   stat_lineribbon(alpha = 0.4, .width = c(0.66, 0.99)) +
#'   scale_y_continuous('Predicted Hazard', labels = scales::comma) +
#'   scale_fill_brewer() +
#'   theme_minimal()
#'
#' # ---- summarize sampling quality ----
#'
#' library(posterior)
#' d <- fetch_predicted_hazard(run_id, level = 'overall', return = 'draws')
#'
#' d %>% spread(.variable, .value) %>%
#'    group_by(.level, survival_time, .type, run_id) %>%
#'    group_modify(~ summarise_draws(.x))
#' }
#'
#' @param run_id (str) [required] One or several model run_ids. See \code{\link{find_runs}} for a list of runs available.
#' @param level (str) The level at which to return predicted values. One of: subject, trial_arm, study, or overall. Default value is per study
#' @param return (str) The type of summary to return. One of: median, quantiles, intervals, or draws. Default: median
#' @param type (str) Whether to return posterior or prior predictions. Default: posterior
#' @param project (str) The name of the project to which the run_id belongs.
#' @param project_version_id (str) The specific project_version_id to which the run_id belongs. Defaults to the most recent project_version_id if none provided.
#'
#' @return data.frame in tidy format, with one record per parameter, run_id, and summarized level. See notes for specific details about each return type.
#' @export
fetch_predicted_hazard <- function(run_id,
                                   level = c('study', 'trial_arm', 'subject', 'overall'),
                                   return = c('median', 'quantiles', 'intervals', 'draws'),
                                   type = c('posterior', 'prior'),
                                   project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'hazard', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id, level = level)
}

#' Fetch biomarker sub-model parameters (f, kg, ks)
#'
#' Fetch inferences for biomarker sub-model parameters from a specific model fit
#'
#' Inferences from the biomarker sub-model can be provided at different levels of the hierarchical model:
#' \enumerate{
#'   \item Per subject
#'   \item Per trial_arm
#'   \item Overall
#' }
#'
#' Use the `level` argument to select the desired level at which to summarize predicted values.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' @note
#' The columns returned depends on the value of the `return` argument. The default is to return median values for each \code{level} and parameter (\code{.variable}).
#'
#' All return formats share a set of columns containing meta-information about the predicted quantities:
#' \enumerate{
#'   \item \code{.variable} Text label for the predicted quantity or variable. In this case, there will be three values: "kg", "ks", and "f"
#'   \item \code{run_id} Text field containing the run_id from which each returned value was generated.
#'   \item \code{.type} Text field containing the type (prior or posterior) of predicted quantity or inferences summarized.
#'   \item \code{.level} Text field containing the level (subject, trial_arm, or overall) at which the predicted values were prepared.
#'   \item \code{subject_id | trial_arm_id } If returning predicted values at the subject, study, or trial-arm level, the ids corresponding to the subjects or trial-arms used in the prediction.
#' }
#'
#' In addition, there will be a few columns to provide and describe the predicted quantities.
#' The set of columns included here will depend on the `return` argument:
#' \enumerate{
#'   \item if \code{return  == 'median'}, a pair of columns: \code{.value} and \code{.point}
#'   \item if \code{return == 'quantiles'}, a pair of columns: \code{.value} and \code{quantile}
#'   \item if \code{return == 'intervals'}, a set of columns: \code{.value}, \code{.width}, \code{.lower}, and \code{.upper} containing the median estimate (.value) along with the lower (.lower) and upper (.upper) bounds for the 50, 80, and 90 percent credible intervals (.width).
#'      \itemize{
#'        \item In addition, columns \code{.point} and \code{.interval} describe the type of point estimate ('median') and interval ('qi')
#'        \item This data structure mimics that returned by \code{\link[tidybayes:median_qi]{median_qi}} function in the \code{\link[tidybayes:tidybayes-package]{tidybayes}} package.
#'        }
#'   \item if \code{return == 'draws'}, a set of columns: \code{.value}, \code{.chain}, \code{.iteration}, and \code{.draw} describing the predicted quantities for each draw, chain and iteration. This data structure mimics the \code{\link[posterior:draws_df]{draws_df}} format from the \code{\link[posterior:posterior-package]{posterior}} package.
#' }
#'
#' @examples
#' \dontrun{
#' library(tidybayes)
#' library(tidyverse)
#'
#' login()
#'
#' # ---- Re-compute SLD_hat using overall parameter estimates ----
#' d <- fetch_biomarker_params(run_id, level = 'overall')
#'
#' # compute log-sld-hat overall from parameters:
#' stein_fojo <- function(bas_sld, f, kg, ks, t) {
#'   sld_hat <- dplyr::case_when(t < 0 ~ bas_sld * exp(kg * t),
#'                               t == 0 ~ bas_sld,
#'                               t > 0 ~ bas_sld * ((1-f) * exp(kg * t) + f * exp(-1*ks * t)))
#'                               }
#'
#' d %>%
#'   spread(.variable, .value) %>%
#'   select(f, kg, ks) %>%
#'   expand_grid(t = seq(from = -10, to = 500, by = 10)) %>%
#'   dplyr::mutate(sld_hat = stein_fojo(f = f, kg = kg, ks = ks, t = t, bas_sld = 1))
#'
#' # ---- summarize sampling quality for trial-arm-level parameters ----
#'
#' library(posterior)
#' d <- fetch_biomarker_params(run_id, level = 'trial_arm', return = 'draws')
#'
#' d %>% spread(.variable, .value) %>%
#'    group_by(.level, trial_arm_id, .type, run_id) %>%
#'    group_modify(~ summarise_draws(.x))
#' }
#'
#' @param run_id (str) [required] One or several model run_ids. See \code{\link{find_runs}} for a list of runs available.
#' @param level (str) The level at which to return predicted values. One of: subject, trial_arm, or overall. Default value is per subject.
#' @param return (str) The type of summary to return. One of: median, quantiles, intervals, or draws. Default: median
#' @param type (str) Whether to return posterior or prior predictions. Default: posterior
#' @param project (str) The name of the project to which the run_id belongs.
#' @param project_version_id (str) The specific project_version_id to which the run_id belongs. Defaults to the most recent project_version_id if none provided.
#'
#' @return data.frame in tidy format, with one record per parameter, run_id, and summarized level. See notes for specific details about each return type.
#' @export
fetch_biomarker_params <- function(run_id,
                                   level = c('subject', 'trial_arm', 'overall'),
                                   return = c('median', 'quantiles', 'intervals', 'draws'),
                                   type = c('posterior', 'prior'),
                                   project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'biomarker_params', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id, level = level)
}

#' Fetch inferences for association states
#'
#' Fetch inferences for association states derived from the SLD sub-model parameters from a specific model fit
#'
#' Inferences for association states can be provided at different levels of the hierarchical model:
#' \enumerate{
#'   \item Per subject
#'   \item Per trial_arm
#' }
#'
#' Use the `level` argument to select the desired level at which to summarize predicted values.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' @note
#' The columns returned depends on the value of the `return` argument. The default is to return median values for each \code{level} and parameter (\code{.variable}).
#'
#' All return formats share a set of columns containing meta-information about the predicted quantities:
#' \enumerate{
#'   \item \code{.variable} Text label for the predicted quantity or variable. In this case, there will be three values: one per association state estimated
#'   \item \code{run_id} Text field containing the run_id from which each returned value was generated.
#'   \item \code{.type} Text field containing the type (prior or posterior) of predicted quantity or inferences summarized.
#'   \item \code{.level} Text field containing the level (subject, trial_arm, or overall) at which the predicted values were prepared.
#'   \item \code{subject_id | trial_arm_id } If returning predicted values at the subject, study, or trial-arm level, the ids corresponding to the subjects or trial-arms used in the prediction.
#' }
#'
#' In addition, there will be a few columns to provide and describe the predicted quantities.
#' The set of columns included here will depend on the `return` argument:
#' \enumerate{
#'   \item if \code{return  == 'median'}, a pair of columns: \code{.value} and \code{.point}
#'   \item if \code{return == 'quantiles'}, a pair of columns: \code{.value} and \code{quantile}
#'   \item if \code{return == 'intervals'}, a set of columns: \code{.value}, \code{.width}, \code{.lower}, and \code{.upper} containing the median estimate (.value) along with the lower (.lower) and upper (.upper) bounds for the 50, 80, and 90 percent credible intervals (.width).
#'      \itemize{
#'        \item In addition, columns \code{.point} and \code{.interval} describe the type of point estimate ('median') and interval ('qi')
#'        \item This data structure mimics that returned by \code{\link[tidybayes:median_qi]{median_qi}} function in the \code{\link[tidybayes:tidybayes-package]{tidybayes}} package.
#'        }
#'   \item if \code{return == 'draws'}, a set of columns: \code{.value}, \code{.chain}, \code{.iteration}, and \code{.draw} describing the predicted quantities for each draw, chain and iteration. This data structure mimics the \code{\link[posterior:draws_df]{draws_df}} format from the \code{\link[posterior:posterior-package]{posterior}} package.
#' }
#'
#' @examples
#' \dontrun{
#' library(tidybayes)
#' library(tidyverse)
#'
#' login()
#'
#' # ---- Pairs plot of subject states ----
#' subject_states <- fetch_association_state(run_id, level = 'subject') %>%
#'          left_join(fetch_subjects() %>%
#'                         distinct(subject_id, trial_arm_id, trial_arm_name),
#'                    by = 'subject_id')
#'
#' ggplot(subject_states %>% spread(association_state, .value),
#'        aes(x = log1p_time_to_baseline_months, y = log1p_time_to_min_sld_months,
#'           colour = trial_arm_name)) +
#'     geom_point()
#'
#' # ---- summarize sampling quality for trial-arm-level parameters ----
#'
#' library(posterior)
#' d <- fetch_association_state(run_id, level = 'trial_arm', return = 'draws')
#'
#' d %>% spread(association_state, .value) %>%
#'    group_by(.level, trial_arm_id, .variable, .type, run_id) %>%
#'    group_modify(~ summarise_draws(.x))
#' }
#'
#' @param run_id (str) [required] One or several model run_ids. See \code{\link{find_runs}} for a list of runs available.
#' @param level (str) The level at which to return predicted values. One of: subject or trial_arm. Default value is per subject.
#' @param return (str) The type of summary to return. One of: median, quantiles, intervals, or draws. Default: median
#' @param type (str) Whether to return posterior or prior predictions. Default: posterior
#' @param project (str) The name of the project to which the run_id belongs.
#' @param project_version_id (str) The specific project_version_id to which the run_id belongs. Defaults to the most recent project_version_id if none provided.
#'
#' @return data.frame in tidy format, with one record per parameter, run_id, and summarized level. See notes for specific details about each return type.
#' @export
fetch_association_state <- function(run_id,
                                   level = c('subject', 'trial_arm'),
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


#' Fetch inferences for hazard betas
#'
#' Fetch inferences for hazard beta parameters - these estimate the relative hazard according to covariate or state values
#'
#' Inferences for hazard are only available at a single level of the hierarchical model:
#' \enumerate{
#'   \item Overall
#' }
#'
#' If/when additional levels are available, you will be able to use the `level` argument to select the desired level at which to summarize predicted values.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the quantiles from the Generable API.
#'
#' @note
#' The columns returned depends on the value of the `return` argument. The default is to return median values for each \code{level} and parameter (\code{.variable}).
#'
#' All return formats share a set of columns containing meta-information about the predicted quantities:
#' \enumerate{
#'   \item \code{.variable} Text label for the predicted quantity or variable. In this case, there will be three values: one per association state estimated
#'   \item \code{run_id} Text field containing the run_id from which each returned value was generated.
#'   \item \code{.type} Text field containing the type (prior or posterior) of predicted quantity or inferences summarized.
#'   \item \code{.level} Text field containing the level (subject, trial_arm, or overall) at which the predicted values were prepared.
#'   \item \code{beta_category} The name of the type of beta estimate. For example, 'smoking_exposure'
#'   \item \code{beta_value} The value or term within each beta category. For example: 'never', 'current', 'former'
#' }
#'
#' In addition, there will be a few columns to provide and describe the predicted quantities.
#' The set of columns included here will depend on the `return` argument:
#' \enumerate{
#'   \item if \code{return  == 'median'}, a pair of columns: \code{.value} and \code{.point}
#'   \item if \code{return == 'quantiles'}, a pair of columns: \code{.value} and \code{quantile}
#'   \item if \code{return == 'intervals'}, a set of columns: \code{.value}, \code{.width}, \code{.lower}, and \code{.upper} containing the median estimate (.value) along with the lower (.lower) and upper (.upper) bounds for the 50, 80, and 90 percent credible intervals (.width).
#'      \itemize{
#'        \item In addition, columns \code{.point} and \code{.interval} describe the type of point estimate ('median') and interval ('qi')
#'        \item This data structure mimics that returned by \code{\link[tidybayes:median_qi]{median_qi}} function in the \code{\link[tidybayes:tidybayes-package]{tidybayes}} package.
#'        }
#'   \item if \code{return == 'draws'}, a set of columns: \code{.value}, \code{.chain}, \code{.iteration}, and \code{.draw} describing the predicted quantities for each draw, chain and iteration. This data structure mimics the \code{\link[posterior:draws_df]{draws_df}} format from the \code{\link[posterior:posterior-package]{posterior}} package.
#' }
#'
#' @examples
#' \dontrun{
#' library(tidybayes)
#' library(tidyverse)
#'
#' login()
#'
#' # ---- Pairs plot of subject states ----
#' hazard_betas <- fetch_hazard_betas(run_id, return = 'intervals')
#'
#' ggplot(hazard_betas, aes(x = .value, xmin = .lower, xmax = .upper, y = beta_value)) +
#'   geom_pointinterval() +
#'   facet_grid(beta_category ~ ., scale = 'free_y')
#'
#'
#' # ---- summarize sampling quality for hazard beta parameters ----
#'
#' library(posterior)
#' d <- fetch_hazard_betas(run_id, return = 'draws')
#'
#' d %>%
#'    group_by(.level, beta_category, beta_value, .variable, .type, run_id) %>%
#'    group_modify(~ summarise_draws(.x))
#' }
#'
#' @param run_id (str) [required] One or several model run_ids. See \code{\link{find_runs}} for a list of runs available.
#' @param level (str) The level at which to return predicted values. Only Overall level available.
#' @param return (str) The type of summary to return. One of: median, quantiles, intervals, or draws. Default: median
#' @param type (str) Whether to return posterior or prior predictions. Default: posterior
#' @param project (str) The name of the project to which the run_id belongs.
#' @param project_version_id (str) The specific project_version_id to which the run_id belongs. Defaults to the most recent project_version_id if none provided.
#'
#' @return data.frame in tidy format, with one record per parameter, run_id, and summarized level. See notes for specific details about each return type.
#' @export
fetch_hazard_betas <- function(run_id,
                                    level = c('overall'),
                                    return = c('median', 'quantiles', 'intervals', 'draws'),
                                    type = c('posterior', 'prior'),
                                    project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  level <- match.arg(level)
  return <- match.arg(return)
  type <- match.arg(type)

  parlist <- .get_pars_by_type(type = 'hazard_betas', level = level, project_version_id = pv_id)
  .fetch_pars_by_type(parlist = parlist, return = return, run_id = run_id, type = type, project_version_id = pv_id, level = level) %>%
    tidyr::gather('beta_category', 'beta_value', .data$smoking_exposure, .data$association_state) %>%
    dplyr::filter(!is.na(.data$beta_value))
}


.fetch_pars_by_type <- function(parlist, return, run_id, type = c('posterior', 'prior'), level, project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  checkmate::assert_choice(return, choices = .VALID_RETURN_TYPES)
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
  d <- parlist %>%
    purrr::map(fetchfun) %>%
    purrr::imap_dfr(~ dplyr::mutate(.x, .variable = .y))
  if (level == 'trial_arm') {
    d <- d %>%
      dplyr::semi_join(fetch_subjects(project_version_id = pv_id) %>%
                         dplyr::distinct(.data$trial_arm_id),
                       by = 'trial_arm_id')
  } else if (level == 'subject') {
    d <- d %>%
      dplyr::semi_join(fetch_subjects(project_version_id = pv_id) %>%
                         dplyr::distinct(.data$subject_id),
                       by = 'subject_id')
  } else if (level == 'study') {
    d <- d %>%
      dplyr::rename(trial_id = .data$study_id) %>%
      dplyr::semi_join(fetch_subjects(project_version_id = pv_id) %>%
                         dplyr::distinct(.data$trial_id),
                       by = 'trial_id')
  }
  d
}

.fetch_par_base <- function(par, trans, level, run_id, project_version_id, type, fetch_fun) {
  d <- fetch_fun(parameter = par, project_version_id = project_version_id, type = type, run_id = run_id, quiet = TRUE)
  if (!is.null(trans)) {
    d <- d %>%
      dplyr::mutate(.value = trans(.data$.value))
  }
  d %>%
    dplyr::mutate(.type = !!type) %>%
    dplyr::mutate(.level = !!level)
}

.fetch_par_quantiles <- function(par, trans, level, run_id, project_version_id, type) {
  .fetch_par_base(par = par, trans = trans, run_id = run_id, project_version_id = project_version_id, type = type, level = level,
                  fetch_fun = fetch_quantiles)
}

.fetch_par_draws <- function(par, trans, level, run_id, project_version_id, type) {
  .fetch_par_base(par = par, trans = trans, run_id = run_id, project_version_id = project_version_id, type = type, level = level,
                  fetch_fun = fetch_draws)
}

.fetch_par_intervals <- function(par, trans, level, run_id, project_version_id, type) {
  .fetch_par_quantiles(par = par, trans = trans, run_id = run_id, project_version_id = project_version_id, type = type, level = level) %>%
    format_quantiles_as_widths()
}

.fetch_par_median <- function(par, trans, level, run_id, project_version_id, type) {
  .fetch_par_quantiles(par = par, trans = trans, run_id = run_id, project_version_id = project_version_id, type = type, level = level) %>%
    dplyr::filter(.data$quantile == 0.5) %>%
    dplyr::select(-.data$quantile) %>%
    dplyr::mutate(.point = 'median')
}

