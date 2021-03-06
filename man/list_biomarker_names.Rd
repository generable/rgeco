% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geco_biomarkers.R
\name{list_biomarker_names}
\alias{list_biomarker_names}
\title{List distinct biomarker measurement names for a project}
\usage{
list_biomarker_names(project = NULL, project_version_id = NULL)
}
\arguments{
\item{project}{Project name}

\item{project_version_id}{Project version. If this is specified, the `project` argument is ignored.}
}
\value{
vector of measurement names where there is at least one observation in the proejct
}
\description{
List distinct biomarker measurement names for a project
}
