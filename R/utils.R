#' Filter the data to include only `n` groups
#' @param d data.frame from which to sample
#' @param n number of groups to sample
#' @param ... unquoted variable names defining groups
#' @param replace boolean, whether to sample with replacement (default: F)
#' @return subset of data with `n` groups selected
#' @importFrom dplyr select distinct sample_n semi_join
#' @importFrom rlang ensyms !!!
#' @export
#' @examples
#'  \dontrun{
#'  # plot data for a random individual, defined by `individual_id`
#'  d %>%
#'  sample_groups(n = 1, individual_id) %>%
#'  ggplot(., aes(...))
#'  }
sample_groups <- function(d, n, ..., replace = F) {
  group_vars <- rlang::ensyms(...)
  d %>%
    dplyr::semi_join(d %>%
                       dplyr::select(!!!group_vars) %>%
                       dplyr::distinct() %>%
                       dplyr::sample_n(size = n, replace = replace))
}
