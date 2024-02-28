
.fetch_treatments_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  regimens <- geco_api(REGIMENS, project_version_id = pv_id)
  .format_treatments_data(regimens)
}

.format_treatments_data <- function(regimens) {
  treatments <- regimens$content %>%
    purrr::set_names(purrr::map_chr(regimens$content, 'id')) %>%
    purrr::map('treatments')

  td <- purrr::map_dfr(treatments, ~ as_dataframe.geco_api_data(content = .x, flatten_names = 'drug'), .id = 'regimen_id') %>%
    tidyr::unnest_wider(.data$drug, names_sep = '_', names_repair = 'universal') %>%
    dplyr::rename_at(.vars = dplyr::vars(-dplyr::starts_with('drug'),
                                         -dplyr::starts_with('regimen')),
                     .funs = .add_prefix,
                     prefix = 'treatment')
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
.fetch_regimens_data <- function(project = NULL, project_version_id = NULL) {
  pv_id <- .process_project_inputs(project = project, project_version_id = project_version_id)
  ret <- geco_api(REGIMENS, project_version_id = pv_id)
  rd <- ret$content %>%
    .as_nested_data() %>%
    dplyr::rename_all(.add_prefix, prefix = 'regimen')
  # format treatments & drugs, as these are important for this result
  td <- .format_treatments_data(ret) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$regimen_id) %>%
    dplyr::mutate(regimen_type = .format_treatment_type(.data$drug_treatment_type)) %>%
    dplyr::ungroup() %>%
    tidyr::nest(regimen_drugs = c(dplyr::starts_with('drug'), dplyr::starts_with('treatment')))
  rd <- rd %>% dplyr::left_join(td, by = 'regimen_id')
  rd
}

