[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rfPermute)](https://cran.r-project.org/package=rfPermute)
[![](http://cranlogs.r-pkg.org/badges/grand-total/rfPermute)](https://cran.r-project.org/package=rfPermute)
[![Travis-CI Build Status](https://travis-ci.org/EricArcher/rfPermute.svg?branch=master)](https://travis-ci.org/EricArcher/rfPermute)

# rfPermute

## Description

*rfPermute* estimates the significance of importance metrics for a Random Forest model by permuting the response variable. It will produce null distributions of importance metrics for each predictor variable and p-value of observed. The package also includes several summary and 
visualization functions for `randomForest` and `rfPermute` results.

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

## Changes in 2.1.1:

* Added `n` argument to `impHeatmap`.
* Added functions: `classConfInt`, `confusionMatrix`, `plotVotes`, `pctCorrect`.

## Changes in 2.0.1:

* Fixed bug in plot.rfPermute that was reporting the p-value incorrectly at the top of the figure.
* Fixed multi-threading in rfPermute so it works on Windows too.
* Added impHeatmap function.
* Switched proximity.plot to use ggplot2 graphics.

## Changes in 2.0:

* Fixed bug with calculation of p-values not respecting importance measure scaling (division by standard deviations). New format of output of `rfPemute` has separate `$null.dist` and `$pval` elements, each with results for unscaled and scaled importance mesures. See ?rfPermute for more information.
* `rp.importance` and `plot.rfPermute` now take a `scale` argument to specify whether or not importance values should be scaled by standard deviations.
* If `nrep = 0` for `rfPermute`, a `randomForest` object is returned.

## Changes in 1.9.3:

* Fixed import declarations to avoid grid name clashes
* Fixed logic error in clean.rf.data where fixed predictors were not removed
* Fixed error in use of main argument in plot.rp.importance

## Changes in 1.9.2:

* Added this NEWS.md
* Added README.md
* Added num.cores argument to rfPermute to take advantage of multi-threading 

## Changes in 1.9.1:

* Added internal keyword to calc.imp.pval to keep it from indexing
* Updated imports to match new CRAN policies