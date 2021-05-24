# rgeco
R client package for access to Generable compute (Geco) API. The API gives you access to the same data, inferences, and analytical summaries displayed in the application.

You must have a user account with Generable to use this package. See https://generable.com for more info or to request a demo.

## Quick start for data

Install the package via [remotes](https://remotes.r-lib.org/):

```r
library(remotes)
remotes::install_github('generable/rgeco')
```

Login with your Generable credentials:

```r
library(rgeco)
rgeco::login(user = 'user@email.com', password = 'yourpassword')
```

List projects available:

```r
projects <- list_projects()
```

Read subjects or biomarkers data for a project (replace `demo` with the name of a project you have access to)

```r
subjects <- fetch_subjects(project = 'demo')
biomarkers <- fetch_biomarkers(project = 'demo', measurement_name = 'sld')
```

Read dosing data

```r
doses <- fetch_doses(project = 'demo')
```

Read formatted PKPD data

```r
measures <- fetch_measurement_names(project = 'demo')
pkpd <- fetch_pkpd(project = 'demo', pd_measure = 'name_of_pd_measure', pk_measure = 'concentration')
```

List versions available for a project:

```r
versions <- list_project_versions(project = 'demo')
```

