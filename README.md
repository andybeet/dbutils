# dbutils

<!-- badges: start -->
[![R-CMD-check](https://github.com/andybeet/dbutils/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/andybeet/dbutils/actions/workflows/check-standard.yaml)
[![gh-pages](https://github.com/andybeet/dbutils/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/andybeet/dbutils/actions/workflows/pkgdown.yaml)
[![gitleaks](https://github.com/andybeet/dbutils/actions/workflows/secretScan.yml/badge.svg)](https://github.com/andybeet/dbutils/actions/workflows/secretScan.yml)
<!-- badges: end -->

Collection of utility function to aid in database manipulation

## Usage


### Installation

You will need to have Oracle's instant client installed and set up on your machine then:

`remotes::install_github("andybeet/dbutils")`

If this fails to install then you will need to get `ROracle` installed. The current version of `ROracle` on CRAN is out of date and has not been tested with versions of R > 3.2 and versions of instant client > 12.x. 


## Usage

To connect to databases:

`con <- dbutils::connect_to_database("dbname","username")`

or

`con <- dbutils::connect_to_database("dbname","username",ROracle=F)` 

if you want to use `odbc` to make a db connection rather than `ROracle`

`DBI::dbGetQuery(con,sqlStatement)`

## Contact

| [andybeet](https://github.com/andybeet)                                                         |
|-------------------------------------------------------------------------------------------------|
| [![](https://avatars1.githubusercontent.com/u/22455149?s=100&v=4)](https://github.com/andybeet) |

#### Legal disclaimer

*This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.*

