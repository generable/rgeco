% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inference_dataset.R
\name{list_datasets}
\alias{list_datasets}
\title{List datasets from the Generable API}
\usage{
list_datasets(project = NULL, project_version_id = NULL)
}
\arguments{
\item{project}{Project name. If NULL, defaults to value of environment variable GECO_API_PROJECT}

\item{project_version_id}{Project version. If NULL, defaults to the most recent version of the project if provided, or the value of environment variable GECO_API_PROJECT_VERSION}
}
\value{
data.frame of metadata for all datasets for the project specified
}
\description{
List datasets from the Generable API for a specific project.
}
\details{
A `dataset` is used by a model to generate a run. This function retrieves
the metadata about all datasets within a project version.

Authentication (see \code{\link{login}}) is required prior to using this function
and this pulls the metadata from the Generable API.
}
\note{
A project can be specified by using the project name or a specific project version.
\enumerate{
  \item If a project is specified using the name, data is fetched for the latest version of the project.
  \item If a project is specified using the project version, the project name is not required.
  \item If neither a project nor a project version is provided, the default project or project version is used. These are set by the environment variables GECO_API_PROJECT and GECO_API_PROJECT_VERSION
}
}
