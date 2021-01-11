# rgeco
R client package for access to Generable compute (Geco) API. The API gives you access to the same data, inferences, and analytical summaries displayed in the application.

You must have a licensed user account with Generable to use this package. See http://generable.com for more info or to request a demo.

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
projects <- get_geco_projects()
```

Read subjects or biomarkers data for a project

```r
subjects <- get_geco_subjects(project = 'demo')
biomarkers <- get_geco_biomarkers(project = 'demo', measurement_name = 'sld')
```

Read dosing data

```r
doses <- get_geco_doses(project = 'demo')
```

List versions available for a project:

```r
versions <- get_geco_projectversions(project = 'demo')
```

