
test_login <- function() {
  if (is.null(Sys.getenv('GECO_API_USER')) || Sys.getenv('GECO_API_USER') == '') {
    testthat::skip('test credentials not supplied.')
  }
  futile.logger::flog.info('Logging in as test user ...')
  a <- login(Sys.getenv('GECO_API_USER'), password = Sys.getenv('GECO_API_PASSWORD'))
}

TEST_PROJECT <- Sys.getenv('GECO_API_TEST_PROJECT')
