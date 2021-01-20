
.get_geco_treatments_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  regimens <- geco_api(REGIMENS, project_version_id = pv_id)
  .format_treatments_data(regimens)
}

.format_treatments_data <- function(regimens) {
  treatments <- regimens$content %>% purrr::set_names(purrr::map_chr(regimens$content, 'id')) %>% purrr::map('treatments')
  td <- purrr::map_dfr(treatments, ~ as_dataframe.geco_api_data(content = .x, flatten_names = 'drug'), .id = 'regimen_id') %>%
    dplyr::mutate(drug = dplyr::rename_all(drug, .add_prefix, prefix = 'drug')) %>%
    dplyr::rename_at(.vars = dplyr::vars(.data$id, .data$created_at), .funs = .add_prefix, prefix = 'treatment')
  td_drugs <- td$drug
  td <- td %>%
    dplyr::select(-drug) %>%
    dplyr::bind_cols(td_drugs)
  td
}

#' @importFrom magrittr %>%
.get_geco_regimens_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  regimens <- geco_api(REGIMENS, project_version_id = pv_id)
  rd <- regimens %>%
    as_dataframe.geco_api_data(flatten_names = c('treatments$drug')) %>%
    dplyr::select(-treatments) %>%
    dplyr::distinct() %>%
    dplyr::rename_all(.add_prefix, prefix = 'regimen')
  td <- .format_treatments_data(regimens) %>%
    dplyr::distinct()
  rd <- rd %>% dplyr::left_join(td, by = 'regimen_id')
  rd
}

