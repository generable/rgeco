
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
fetch_inference_draws <- function(project = NULL, project_version_id = NULL, run_id, parameter, type = c('posterior', 'prior')) {
  type <- match.arg(type, several.ok = F)
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  draws <- geco_api(IDRAWS, project_version_id = pv_id, run_id=run_id, parameter=parameter, type=type)
  d <- convert_draws_to_df(draws)
}

