
#' Fetch model attributes from the Generable API
#'
#' Fetch model attributes from the Generable API for a specific project.
#'
#' A model is used to generate a run. This function retrieves the attributes about
#' all models within a project version with at least one run. 
#'
#' Authentication (see \code{\link{login}}) is required prior to using this function
#' and this pulls the metadata from the Generable API.
#'
#' A project can be specified by using the project name or a specific project version.
#' If a project is specified using the name, data is fetched for the latest version of the project.
#' If a project is specified using the project version, the project name is ignored if it
#' is also included as an argument.
#'
#' @param project Project name
#' @param project_version_id Project version. If this is specified, the `project` argument is ignored.
#' @return data.frame of model attributes for the project specified
#'
#' @importFrom magrittr %>%
#' @export
fetch_inference_models <- function(project = NULL, project_version_id = NULL) {
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



