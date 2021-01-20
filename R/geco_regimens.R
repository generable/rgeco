
.get_geco_treatments_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  regimens <- geco_api(REGIMENS, project_version_id = pv_id)
  .format_treatments_data(regimens)
}

.format_treatments_data <- function(regimens) {
  treatments <- regimens$content %>% purrr::set_names(purrr::map_chr(regimens$content, 'id')) %>% purrr::map('treatments')
  td <- purrr::map_dfr(treatments, ~ as_dataframe.geco_api_data(content = .x, flatten_names = 'drug'), .id = 'regimen_id') %>%
    dplyr::mutate(drug = dplyr::rename_all(.data$drug, .add_prefix, prefix = 'drug')) %>%
    dplyr::rename_at(.vars = dplyr::vars(.data$id, .data$created_at), .funs = .add_prefix, prefix = 'treatment')
  td_drugs <- td$drug
  td <- td %>%
    dplyr::select(-.data$drug) %>%
    dplyr::bind_cols(td_drugs)
  td
}

.format_treatment_type <- function(types) {
  is_multiple <- length(types) > 1
  types <- sort(unique(types)) %>%
    stringr::str_c(collapse = "+")
  stopifnot(length(types) == 1)
  if (isTRUE(is_multiple)) {
    stringr::str_c('combination ', types)
  } else {
    types
  }
}

#' @importFrom magrittr %>%
.get_geco_regimens_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  regimens <- geco_api(REGIMENS, project_version_id = pv_id)
  rd <- regimens %>%
    as_dataframe.geco_api_data(flatten_names = c('treatments$drug')) %>%
    dplyr::select(-.data$treatments) %>%
    dplyr::distinct() %>%
    dplyr::rename_all(.add_prefix, prefix = 'regimen')
  td <- .format_treatments_data(regimens) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$regimen_id) %>%
    dplyr::mutate(regimen_type = .format_treatment_type(.data$drug_treatment_type)) %>%
    dplyr::ungroup() %>%
    tidyr::nest(regimen_drugs = c(dplyr::starts_with('drug'), dplyr::starts_with('treatment')))
  rd <- rd %>% dplyr::left_join(td, by = 'regimen_id')
  rd
}

