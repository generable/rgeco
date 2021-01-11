
futile.logger::flog.info('Logging in as test user ...')
a <- login(Sys.getenv('GECO_API_USER'), password = Sys.getenv('GECO_API_PASSWORD'))
TEST_PROJECT <- Sys.getenv('GECO_API_TEST_PROJECT')
