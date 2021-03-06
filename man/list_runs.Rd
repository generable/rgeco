% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inference_runs.R
\name{list_runs}
\alias{list_runs}
\title{List runs from the Generable API}
\usage{
list_runs(project = NULL, project_version_id = NULL)
}
\arguments{
\item{project}{Project name. If NULL, defaults to value of environment variable GECO_API_PROJECT}

\item{project_version_id}{Project version. If NULL, defaults to the most recent version of the project if provided, or the value of environment variable GECO_API_PROJECT_VERSION}
}
\value{
data.frame of run attributes for the project specified
}
\description{
List runs from the Generable API for a specific project.
}
\details{
A run is generated by a model and a dataset. This function retrieves the
attributes about all runs within a project version. The returned `data.frame`
contains information about what draws and quantiles are available and the model
and dataset ids.

The `run_draws` and `run_quantiles` columns contain two named lists of `parameter_names`
and `predictive_names`. The `parameter_names` is a list of the parameters of the model, e.g.
the sld parameters f, kg, and ks. The `predictive_names` is a list of the predicted quantities,
e.g. the predicted survival for each trial arm `predicted_survival_per_trial_arm`.

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
\seealso{
\code{\link{list_models}}, \code{\link{list_datasets}},
         \code{\link{fetch_quantiles}}, \code{\link{fetch_draws}}
}
