
LOGIN <- 'users/login'

# ---- data api endpoints ----
TRIALS <- 'data/projectversion/{project_version_id}/trials'
TRIALARMS <- 'data/projectversion/{project_version_id}/trialarms'
SUBJECTS <- 'data/projectversion/{project_version_id}/subjects'
LABS <- 'data/projectversion/{project_version_id}/labs'
EVENTS <- 'data/projectversion/{project_version_id}/events'
DOSE <- 'data/projectversion/{project_version_id}/dose'
TIMEVARYING <- 'data/projectversion/{project_version_id}/tvs'
REGIMENS <- 'data/projectversion/{project_version_id}/regimens'
PROJECTVERSIONS <- 'data/project/{project}/projectversions'
PROJECTS <- 'data/projects'

# ---- inference api endpoints ----
IDATA <- 'inferences/projectversion/{project_version_id}/dataset/attributes'
IMODELS <- 'inferences/projectversion/{project_version_id}/models'
IRUNS <- 'inferences/projectversion/{project_version_id}/runs'
IRUNDATA <- 'inferences/projectversion/{project_version_id}/run/{run_id}/dataset'
IDRAWS <- 'inferences/projectversion/{project_version_id}/run/{run_id}/draws/{type}/{parameter}'
IPDRAWS <- 'inferences/projectversion/{project_version_id}/run/{run_id}/draws/{type}/{parameter}/predictive'
ITILES <- 'inferences/projectversion/{project_version_id}/run/{run_id}/quantiles/{type}/{parameter}'
IPTILES <- 'inferences/projectversion/{project_version_id}/run/{run_id}/quantiles/{type}/{parameter}/predictive'
ENV <- new.env(parent = emptyenv())

#' Formatted URL for api endpoints
#' @param url_query_parameters named list of url query parameters
#' @param ... path elements to the url
#' @param project (str) project name
#' @param project_version_id (str) project version id, if project not provided
#' @param run_id (str) the run_id, if used by the URL path
#' @param parameter (str) the parameter, if used by the URL path
#' @param type (str) the type as either prior or posterior, if used by the URL path
#' @param url_query_parameters (named list) other inputs to the query passed as GET params
#' @importFrom glue glue_safe
#' @importFrom httr modify_url
geco_api_url <- function(..., project = NULL, project_version_id = NULL, run_id=NULL, parameter=NULL, type=NULL,
                         url_query_parameters = NULL) {
  if (Sys.getenv('GECO_API_URL') != '') {
    futile.logger::flog.debug(glue::glue('Default Geco API URL overridden via GECO_API_URL environment variable ({Sys.getenv("GECO_API_URL")})'))
  }
  root <- Sys.getenv('GECO_API_URL', unset = "https://geco.generable.com")
  url <- file.path(root, '/gecoapi/v1', ..., fsep = '/')
  if (length(url_query_parameters) > 0) {
    url <- modify_url(url, query = url_query_parameters)
  }
  glue::glue_safe(url)
}

#' Log in to the Generable API
#'
#' This function logs the user into the Generable API.
#'
#' This function logs the user into the Generable API. The user must log in before calling other functions
#' that require authentication. The authentication token for the API is stored in the rgeco package's environment.
#' The token expires when the Generable API is not access within 30 minutes. If this happens, reauthenticate
#' using this function.
#'
#' When this call is successful, it will return the OAuth 2.0 Bearer Token for the user, invisibly.
#' Otherwise, it will error with an error message.
#'
#' @param user User email address. If not provided, will read the `GECO_API_USER` environment variable.
#' @param password User password. If not provided, will read the `GECO_API_PASSWORD` environment variable.
#' @return The OAuth 2.0 Bearer Token for the Generable API
#' @export
login <- function(user, password) {
  if (missing(user)) {
    user <- Sys.getenv('GECO_API_USER')
  }
  if (missing(password)) {
    password <- Sys.getenv('GECO_API_PASSWORD')
  }
  body <- list(email = user, password = password)
  resp <- geco_api(LOGIN, body = body, encode = 'json', method = 'POST')
  ENV$.GECO_AUTH <- resp$content
  invisible(resp$content)
}

get_latest_version_id <- function(project) {
  get_latest_version(project)$id
}

get_latest_version <- function(project) {
  v <- list_project_versions(project = project)
  v %>%
    dplyr::filter(.data$created_at == max(.data$created_at)) %>%
    as.list()
}

get_auth <- function() {
  if (!exists(envir = ENV, '.GECO_AUTH')) {
    futile.logger::flog.error('Not logged in. Use `login(user, password)` to login.')
  }
  futile.logger::flog.debug('Authorization headers found.')
  httr::add_headers(.headers = unlist(ENV$.GECO_AUTH))
}

#' @import httr
#' @importFrom RJSONIO fromJSON
geco_api <- function(path, ..., method = c('GET', 'POST'), project = NULL, project_version_id = NULL, run_id=NULL, type=NULL, parameter=NULL, url_query_parameters=NULL) {
  url <- geco_api_url(path, project = project, project_version_id = project_version_id, run_id=run_id, type=type, parameter=parameter, url_query_parameters=url_query_parameters)

  ua <- httr::user_agent("https://github.com/generable/rgeco")

  method <- match.arg(method, several.ok = FALSE)
  if (method == 'GET')
    resp <- try(httr::GET(url, ..., get_auth(), ua))
  else if (method == 'POST')
    resp <- try(httr::POST(url, ..., ua))
  #if (httr::http_type(resp) != "application/json") {
  #  stop("API did not return json", call. = FALSE)
  #}
  if (inherits(resp, 'try-error')) {
    stop(glue::glue("Error connecting to API: {url} {print(resp)}"))
  }

  parsed <- try(RJSONIO::fromJSON(httr::content(resp, "text", encoding = 'UTF-8'), simplify = FALSE), silent = T)

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "Geco API request failed [%s: %s]",
        httr::status_code(resp),
        parsed$message
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "geco_api_data"
  )
}

#' @importFrom utils str
print.geco_api_data <- function(x, ...) {
  cat("<Geco ", x$path, ">\n", sep = "")
  if (inherits(x$content, 'try-error')) {
    str(x$response)
  } else {
    str(x$content)
  }
  invisible(x)
}

.add_prefix <- function(x, prefix, sep = '_') {
  stringr::str_c(prefix, x, sep = sep)
}

#' @importFrom magrittr %>%
as_dataframe.geco_api_data <- function(x, content = x$content, flatten_names = 'params') {
  if (length(content) == 0) {
    futile.logger::flog.debug('No results returned.')
    return(tibble::tibble(id = character(0), created_at = character(0)))
  }
  to_flatten <- flatten_names %>%
    purrr::keep(~ .x %in% names(content[[1]]))
  if (length(to_flatten) > 0)
    content <- content %>%
      purrr::map(purrr::map_at, to_flatten, ~ purrr::compact(.x) %>% tibble::as_tibble() %>% list(.))
  d <- content %>%
    purrr::map(purrr::compact) %>%
    purrr::map_dfr(tibble::as_tibble_row)
  if ('created_at' %in% names(d)) {
    d <- d %>%
      dplyr::mutate(created_at = lubridate::ymd_hms(.data$created_at))
  }
  d
}

.process_project_inputs <- function(project = NULL, project_version_id = NULL) {
  # check inputs
  if (!is.null(project) && is.null(project_version_id)) {
    checkmate::check_character(project, len = 1, any.missing = FALSE)
    all_projects <- list_projects()$id
    if (!project %in% all_projects) {
      stop(glue::glue('Project `{project}` could not be found. You have access to the following projects: {glue::glue_collapse(all_projects, sep = ", ", last = ", and ")}'),
           call. = FALSE)
    }
  } else if (is.null(project) && !is.null(project_version_id)) {
    checkmate::check_character(project_version_id, len = 1, any.missing = FALSE)
  } else if (is.null(project) && is.null(project_version_id)) {
    stop("Either project or project_version_id is required.", call. = F)
  } else if (!is.null(project) && !is.null(project_version_id)) {
    warning("Both project and project_version_id were provided. Project input will be ignored.", call. = F)
  }
  # get project_version_id
  if (is.null(project_version_id)) {
    pv <- get_latest_version(project)
    futile.logger::flog.info(glue::glue('Project version id set to {pv$id}'))
    futile.logger::flog.info(glue::glue('Data were last updated {pv$created_at}: {pv$description}'))
    pv_id <- pv$id
  } else {
    pv_id <- project_version_id
  }
  # return pv_id
  pv_id
}

.as_nested_data <- function(content) {
  content %>%
    purrr::map(purrr::compact) %>%
    purrr::map(purrr::map_if, ~ is.list(.x) & length(.x) > 1, ~ list(.x)) %>%
    purrr::map_dfr(tibble::as_tibble_row)
}
