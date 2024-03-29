
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
LESIONS <- 'data/projectversion/{project_version_id}/lesions'
LESIONTV <- 'data/projectversion/{project_version_id}/lesiontvs'

# ---- inference api endpoints ----
IDATA <- 'inferences/projectversion/{project_version_id}/dataset/attributes'
IMODELS <- 'inferences/projectversion/{project_version_id}/models'
IRUNS <- 'inferences/projectversion/{project_version_id}/runs'
IRUNDATA <- 'inferences/projectversion/{project_version_id}/run/{run_id}/dataset'
IDRAWS <- 'inferences/projectversion/{project_version_id}/run/{run_id}/draws/{type}/{parameter}'
FIDRAWS <- 'inferences/projectversion/{project_version_id}/run/{run_id}/draws/{type}/{parameter}/{filters}'
IPDRAWS <- 'inferences/projectversion/{project_version_id}/run/{run_id}/draws/{type}/{parameter}/predictive'
FIPDRAWS <- 'inferences/projectversion/{project_version_id}/run/{run_id}/draws/{type}/{parameter}/predictive/{filters}'
ITILES <- 'inferences/projectversion/{project_version_id}/run/{run_id}/quantiles/{type}/{parameter}'
FITILES <- 'inferences/projectversion/{project_version_id}/run/{run_id}/quantiles/{type}/{parameter}/{filters}'
IPTILES <- 'inferences/projectversion/{project_version_id}/run/{run_id}/quantiles/{type}/{parameter}/predictive'
FIPTILES <- 'inferences/projectversion/{project_version_id}/run/{run_id}/quantiles/{type}/{parameter}/predictive/{filters}'
ENV <- new.env(parent = emptyenv())

#' Formatted URL for api endpoints
#' @param url_query_parameters named list of url query parameters
#' @param ... path elements to the url
#' @param project (str) project name
#' @param project_version_id (str) project version id, if project not provided
#' @param run_id (str) the run_id, if used by the URL path
#' @param parameter (str) the parameter, if used by the URL path
#' @param type (str) the type as either prior or posterior, if used by the URL path
#' @param filters (str) formatted filters, for endpoints that use this in the URL path
#' @param url_query_parameters (named list) other inputs to the query passed as GET params
#' @importFrom glue glue_safe
#' @importFrom httr modify_url
#' @importFrom futile.logger flog.logger
geco_api_url <- function(..., project = NULL, project_version_id = NULL, run_id=NULL, parameter=NULL, type=NULL, filters=NULL,
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


get_auth <- function() {
  if (!exists(envir = ENV, '.GECO_AUTH')) {
    futile.logger::flog.error('Not logged in. Use `login(user, password)` to login.')
  }
  futile.logger::flog.debug('Authorization headers found.')
  httr::add_headers(.headers = unlist(ENV$.GECO_AUTH))
}

#' @import httr
#' @importFrom RJSONIO fromJSON
geco_api <- function(path, ..., method = c('GET', 'POST'), project = NULL, project_version_id = NULL, run_id=NULL, type=NULL, parameter=NULL, filters=NULL, url_query_parameters=NULL) {
  url <- geco_api_url(path, project = project, project_version_id = project_version_id, run_id=run_id, type=type, parameter=parameter, filters=filters, url_query_parameters=url_query_parameters)

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

  if (inherits(parsed, 'try-error')) {
    stop(glue::glue('Unable to connect to the API: [{stringr::str_replace_all(parsed, "[\r\n]" , "")}].'),
         call. = FALSE)
  }
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

.get_project <- function() {
  env_project <- dplyr::na_if(Sys.getenv('GECO_API_PROJECT', unset = NA), '')
  if (is.na(env_project))
    env_project <- NULL
  if (!is.null(env_project))
    futile.logger::flog.info(glue::glue('Project set to {env_project}'))
  return(env_project)
}

.get_project_version <- function() {
  env_project_version <- dplyr::na_if(Sys.getenv('GECO_API_PROJECT_VERSION', unset = NA), '')
  if (is.na(env_project_version))
    env_project_version <- NULL
  return(env_project_version)
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
    checkmate::check_character(project_version_id, len = 1, min.chars = 36, any.missing = FALSE)
  } else if (is.null(project) && is.null(project_version_id)) {
    futile.logger::flog.debug('Neither project nor project_version_id provided. Using ENV variables: GECO_API_PROJECT & GECO_API_PROJECT_VERSION.')
    env_project <- .get_project()
    env_project_version <- .get_project_version()
    if (is.null(env_project) && is.null(env_project_version)) {
      futile.logger::flog.debug('Neither project nor project_version_id provided. Please provide either a project or project_version_id, or set the ENV variables (GECO_API_PROJECT & GECO_API_PROJECT_VERSION).')
      stop()
    } else {
      return(.process_project_inputs(project = env_project, project_version_id = env_project_version))
    }
  } else if (!is.null(project) && !is.null(project_version_id)) {
    all_versions <- list_project_versions(project) %>% dplyr::pull(.data$id)
    if (!project_version_id %in% all_versions) {
      stop(glue::glue("Provided project_version_id `{project_version_id}`",
                      " is not a valid project version for project {project}.",
                      "\nRun `list_project_versions('{project}')` to review versions for this project."
                      )
           )
    }
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
