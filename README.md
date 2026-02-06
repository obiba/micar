# Mica R

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/micar)](https://cran.r-project.org/package=micar)
[![R-CMD-check](https://github.com/obiba/micar/actions/workflows/ci.yml/badge.svg)](https://github.com/obiba/micar/actions/workflows/ci.yml)

R package for accessing Mica2 web services:
* search any type of documents (variable, dataset, study, population, dce (datacollection event), network)
* search taxonomy vocabularies and terms
* get data access requests form configuration and listing for reporting

## Installation

```
# Install from CRAN
install.package("micar")
```
## Usage

Steps:

* open connection to Mica server
* extract and process documents
* close connection with Mica server

Examples: 

* [search networks, studies, datasets and variables](https://github.com/obiba/micar/blob/master/inst/examples/mica-search.R)
* [get data access requests](https://github.com/obiba/micar/blob/master/inst/examples/mica-dar.R)

