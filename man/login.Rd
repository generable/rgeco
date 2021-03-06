% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geco_api.R
\name{login}
\alias{login}
\title{Log in to the Generable API}
\usage{
login(user, password)
}
\arguments{
\item{user}{User email address. If not provided, will read the `GECO_API_USER` environment variable.}

\item{password}{User password. If not provided, will read the `GECO_API_PASSWORD` environment variable.}
}
\value{
The OAuth 2.0 Bearer Token for the Generable API
}
\description{
This function logs the user into the Generable API.
}
\details{
This function logs the user into the Generable API. The user must log in before calling other functions
that require authentication. The authentication token for the API is stored in the rgeco package's environment.
The token expires when the Generable API is not access within 30 minutes. If this happens, reauthenticate
using this function.

When this call is successful, it will return the OAuth 2.0 Bearer Token for the user, invisibly.
Otherwise, it will error with an error message.
}
