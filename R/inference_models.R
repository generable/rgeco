
#' List models from the Generable API
#'
#' List model attributes from the Generable API for a specific project.
#'
#' A model is used to generate a run. This function retrieves the attributes about
#' all models within a project version with at least one run.
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the metadata from the Generable API.
#'
#' A project can be specified by using the project name or a specific project version.
#' \enumerate{
#'   \item If a project is specified using the name, data is fetched for the latest version of the project.
#'   \item If a project is specified using the project version, the project name is not required.
#'   \item If neither a project nor a project version is provided, the default project or project version is used. These are set by the environment variables GECO_API_PROJECT and GECO_API_PROJECT_VERSION
#' }
#'
#' @param project Project name. If NULL, defaults to value of environment variable GECO_API_PROJECT
#' @param project_version_id Project version. If NULL, defaults to the most recent version of the project if provided, or the value of environment variable GECO_API_PROJECT_VERSION
#' @return data.frame of model attributes for the project specified
#'
#' @importFrom magrittr %>%
#' @export
list_models <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  models <- geco_api(IMODELS, project_version_id = pv_id)
  if (length(models$content) > 0) {
    d <- models$content %>%
      purrr::map_dfr(tibble::enframe, .id = '.id') %>%
      tidyr::spread(.data$name, .data$value) %>%
      dplyr::select_if(.predicate = ~ all(!is.null(unlist(.x)))) %>%
      tidyr::unnest(cols = c(dplyr::one_of('description', 'hash', 'id', 'inference_engine', 'name', 'type', 'version'))) %>%
      dplyr::select(-.data$.id)
      suppressWarnings({
        d <- d %>%
          dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of(c('run_id'))),
                           .funs = ~ stringr::str_c('model_', .x))
        d <- d %>%
          dplyr::mutate_at(.vars = dplyr::vars(dplyr::one_of('model_parameters', 'model_has_predicted_values')),
                           .funs = ~ purrr::map(.x, unlist))
      })
  } else {
    futile.logger::flog.info('No models returned.')
    d <- tibble::tibble(model_id = character(0))
  }
  d
}



