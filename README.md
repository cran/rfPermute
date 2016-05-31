[![Travis-CI Build Status](https://travis-ci.org/EricArcher/rfPermute.svg?branch=master)](https://travis-ci.org/EricArcher/rfPermute)

# rfPermute

## Description

*rfPermute* estimates the significance of importance metrics for a Random Forest model by permuting the response variable. It will produces null distributions of importance metrics for each predictor variable and p-value of observed.

## Installation

To install the stable version from CRAN:

```r
install.packages('rfPermute')
```

To install the latest version from GitHub:

```r
# make sure you have Rtools installed
if (!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('EricArcher/rfPermute')
```

## Contact

* submit suggestions and bug-reports: <https://github.com/ericarcher/rfPermute/issues>
* send a pull request: <https://github.com/ericarcher/rfPermute/>
* e-mail: <eric.archer@noaa.gov>
