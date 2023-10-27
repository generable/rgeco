#' rgeco: R package for accessing the Generable API
#'
#' The Generable API is a HTTP REST API that provides access
#' to data and inferences. The rgeco package provides convenient
#' access to the information in the API in an R-friendly format.
#'
#' @section Setup:
#'
#' This package requires authentication to the Generable API before
#' you can access data or inferences.
#'
#' You can configure your login credentials as ENV variables by adding
#' them to an \code{.Renviron} file in your project root.
#'
#' The full list of ENV vars supported includes:
#'
#' \preformatted{
#'   GECO_API_USER=
#'   GECO_API_PASSWORD=
#'   GECO_API_URL=https://geco.generable.com
#'   GECO_API_PROJECT=
#'   GECO_API_PROJECT_VERSION=
#' }
#'
#' However, only GECO_API_USER and GECO_API_PASSWORD are required.
#'
#' Once your environment variables are set up, you can run \code{login()} to test authentication.
#'
#' @section Data API:
#'
#' The data namespace provides access to study or project data
#' stored in the Geco database.
#'
#' Key methods for accessing these data are:
#'
#' \enumerate{
#'   \item \code{\link{fetch_biomarkers}}: Fetch biomarkers data
#'   \item \code{\link{fetch_subjects}}: Fetch subjects data
#'   \item \code{\link{fetch_labs}}: Fetch labs data
#'   \item \code{\link{fetch_pkpd}}: Fetch and format pkpd data from biomarkers data
#'   \item \code{\link{list_biomarker_names}}: List biomarkers present in the data
#' }
#'
#' @section Inference API:
#'
#' The inference namespace provides access to inferences from pre-computed \bold{model runs}.
#'
#' A model run is a model fit to a prepared analysis datasets.
#'
#' Key methods for working with inferences are:
#'
#' \enumerate{
#'   \item \code{\link{find_runs}}: List and search runs
#'   \item \code{\link{fetch_dataset}}: Fetch analysis dataset (training data) used for the run
#'   \item \code{\link{fetch_predicted_survival}}: Fetch inferences for predicted survival
#'   \item \code{\link{fetch_predicted_biomarkers}}: Fetch inferences for predicted biomarkers
#'   \item \code{\link{fetch_biomarker_params}}: Fetch inferences for biomarker-submodel parameters
#'   \item \code{\link{fetch_hazard_betas}}: Fetch inferences for relative hazard according to covariates
#'   \item \code{\link{fetch_predicted_hazard}}: Fetch inferences for predicted hazard over time
#'   \item \code{\link{fetch_association_state}}: Fetch inferences for association state(s), linking biomarker model to hazard model
#'   \item \code{\link{list_parameter_names}}: List and describe parameters available for a model run
#'   \item \code{\link{list_predictive_names}}: List and describe predicted quantities available for a model run
#'   \item \code{\link{fetch_draws}}: Fetch draws for any parameter or predicted quantity available for a model run
#'   \item \code{\link{fetch_quantiles}}: Fetch quantiles for any parameter or predicted quantity available for a model run
#' }
#'
#' @docType package
#' @name rgeco
NULL

xarray <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  # check if Python is available
  .check_python_installed()
  .check_python_deps()
  .check_keyring_setup()
}

#' @import cli
#' @importFrom keyring backend_file
.check_keyring_setup <- function() {
  use_keyring <- Sys.getenv('GECO_API_NO_KEYRING') == ""
  if (use_keyring && keyring::has_keyring_support()) {
    cli::cli_inform('This package uses `keyring` to store passwords on your local system securely.')
    cli::cli_alert_success('Keyring supported.')
  }
}

#' @import cli
.check_python_installed <- function() {
  a <- reticulate::py_discover_config()
  if (is.null(a) || length(a)<1) {
    cli::cli_alert_warning('Could not locate Python installation, which will limit ability to query from model results.')
    cli::cli_inform('Try `reticulate::install_miniconda()`, or see https://rstudio.github.io/reticulate/reference/install_miniconda.html')
  } else {
    cli::cli_alert_success(glue::glue('Python installation found (Default: {a$python}).'))
  }
}

.check_python_deps <- function() {
  .install_xarray()
  cli::cli_alert_success('Required Python module (xarray) loaded.')
}

.install_xarray <- function() {
  if (is.null(xarray)) {
    xarray <<- reticulate::import("xarray", convert = FALSE, delay_load = TRUE)
  }
}
