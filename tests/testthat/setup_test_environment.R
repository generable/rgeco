
test_login <- function() {
  if (is.null(Sys.getenv('GECO_API_TEST_USER')) || Sys.getenv('GECO_API_TEST_USER') == '' || Sys.getenv('GECO_API_TEST_URL') == '') {
    testthat::skip('test credentials not supplied.')
  }
  futile.logger::flog.info('Logging in as test user ...')
  Sys.setenv(GECO_API_URL=Sys.getenv('GECO_API_TEST_URL'))
  configure()
  a <- login(Sys.getenv('GECO_API_TEST_USER'), password = Sys.getenv('GECO_API_TEST_PASSWORD'))
}

TEST_PROJECT <- Sys.getenv('GECO_API_TEST_PROJECT')
expect_no_duplicates <- function(.d, by) {
  testthat::expect_equal(.d %>%
                           dplyr::mutate(.dup_check = stringr::str_c(!!!by, sep = ':')) %>%
                           dplyr::filter(duplicated(.dup_check)) %>%
                           nrow(),
                           0, label = glue::glue('N duplicates by {stringr::str_c(purrr::map_chr(by, rlang::as_label), collapse = "+")}'))
}
