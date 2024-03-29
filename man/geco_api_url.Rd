% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geco_api.R
\name{geco_api_url}
\alias{geco_api_url}
\title{Formatted URL for api endpoints}
\usage{
geco_api_url(
  ...,
  project = NULL,
  project_version_id = NULL,
  run_id = NULL,
  parameter = NULL,
  type = NULL,
  filters = NULL,
  url_query_parameters = NULL
)
}
\arguments{
\item{...}{path elements to the url}

\item{project}{(str) project name}

\item{project_version_id}{(str) project version id, if project not provided}

\item{run_id}{(str) the run_id, if used by the URL path}

\item{parameter}{(str) the parameter, if used by the URL path}

\item{type}{(str) the type as either prior or posterior, if used by the URL path}

\item{filters}{(str) formatted filters, for endpoints that use this in the URL path}

\item{url_query_parameters}{(named list) other inputs to the query passed as GET params}
}
\description{
Formatted URL for api endpoints
}
