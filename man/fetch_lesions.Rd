% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geco_lesions.R
\name{fetch_lesions}
\alias{fetch_lesions}
\title{Fetch lesions data from the Generable API}
\usage{
fetch_lesions(project = NULL, project_version_id = NULL, annotate = T, ...)
}
\arguments{
\item{project}{Project name. If NULL, defaults to value of environment variable GECO_API_PROJECT}

\item{project_version_id}{Project version. If NULL, defaults to the most recent version of the project if provided, or the value of environment variable GECO_API_PROJECT_VERSION}

\item{annotate}{whether to format response data}

\item{...}{Optional filters applied to lesions data, provided as name-value pairs to limit returned values.
Example: trial_id = unique(subjects$trial_id)}
}
\value{
data.frame of lesions data for the project specified
}
\description{
Fetch lesion descriptions from the Generable API for a specific project.
}
\details{
This function retrieves lesion-level characteristics from the Generable API.
It requires authentication (see \code{\link{login}}) prior to use
and this pulls data from the Generable API.
}
\note{
A project can be specified by using the project name or a specific project version.
\enumerate{
  \item If a project is specified using the name, data is fetched for the latest version of the project.
  \item If a project is specified using the project version, the project name is not required.
  \item If neither a project nor a project version is provided, the default project or project version is used. These are set by the environment variables GECO_API_PROJECT and GECO_API_PROJECT_VERSION
}
}
