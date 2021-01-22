
TRIALS <- 'geco/projectversion/{project_version_id}/trials'
TRIALARMS <- 'geco/projectversion/{project_version_id}/trialarms'
SUBJECTS <- 'geco/projectversion/{project_version_id}/subjects'
LABS <- 'geco/projectversion/{project_version_id}/labs'
EVENTS <- 'geco/projectversion/{project_version_id}/events'
DOSE <- 'geco/projectversion/{project_version_id}/dose'
TIMEVARYING <- 'geco/projectversion/{project_version_id}/tvs'
REGIMENS <- 'geco/projectversion/{project_version_id}/regimens'
PROJECTVERSIONS <- 'geco/project/{project}/projectversions'
PROJECTS <- 'geco/projects'
LOGIN <- 'users/login'

ENV <- new.env(parent = emptyenv())

#' @importFrom glue glue_safe
geco_api_url <- function(..., project = NULL, project_version_id = NULL) {
  if (Sys.getenv('GECO_API_URL') != '') {
    futile.logger::flog.info(glue::glue('Default Geco API URL overridden via GECO_API_URL environment variable ({Sys.getenv("GECO_API_URL")})'))
  }
  root <- Sys.getenv('GECO_API_URL', unset = "https://dev.generable.com")
  url <- file.path(root, '/gecoapi/v1', ..., fsep = '/')
  glue::glue_safe(url)
}

#' Login to the Generable API
#' @param user (chr) user email. [If not provided, reads from GECO_API_USER environment variable]
#' @param password (chr) user password [If not provided, reads from GECO_API_USER environment variable]
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
  invisible(resp)
}

get_latest_version_id <- function(project) {
  resp <- geco_api(PROJECTVERSIONS, project = project)
  resp$content[[length(resp$content)]]$id
}

get_latest_version <- function(project) {
  v <- get_geco_projectversions(project = project)
  v %>%
    dplyr::filter(created_at == max(created_at)) %>%
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
#' @importFrom jsonlite fromJSON
geco_api <- function(path, ..., method = c('GET', 'POST'), project = NULL, project_version_id = NULL) {
  url <- geco_api_url(path, project = project, project_version_id = project_version_id)

  ua <- httr::user_agent("https://github.com/generable/geco-api")

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

  parsed <- try(jsonlite::fromJSON(httr::content(resp, "text", encoding = 'UTF-8'), simplifyVector = FALSE), silent = T)

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
    warning('No results returned.')
    return(tibble::tibble(id = character(0), created_at = character(0)))
  }
  to_flatten <- flatten_names %>%
    purrr::keep(~ .x %in% names(content[[1]]))
  if (length(to_flatten) > 0)
    content <- content %>%
      purrr::map(purrr::map_at, to_flatten, ~ purrr::compact(.x) %>% tibble::as_tibble())
  d <- content %>%
    purrr::map_dfr(~ purrr::compact(.x) %>% tibble::as_tibble())
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
  } else if (is.null(project) && !is.null(project_version_id)) {
    checkmate::check_character(project_version_id, len = 1, any.missing = FALSE)
  } else if (is.null(project) && is.null(project_version_id)) {
    stop("Either project or project_version_id are required.")
  } else if (!is.null(project) && !is.null(project_version_id)) {
    warning("Both project and project_version_id were provided. Project input will be ignored.")
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
