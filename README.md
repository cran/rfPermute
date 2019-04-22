[![CRAN version](http://www.r-pkg.org/badges/version/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)
[![CRAN last day downloads](http://cranlogs.r-pkg.org/badges/last-day/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)
[![CRAN last week downloads](http://cranlogs.r-pkg.org/badges/last-week/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)
[![CRAN last month downloads](http://cranlogs.r-pkg.org/badges/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)
[![CRAN total downloads](http://cranlogs.r-pkg.org/badges/grand-total/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)  
[![Zenodo DOI](https://zenodo.org/badge/23926/EricArcher/rfPermute.svg)](https://zenodo.org/badge/latestdoi/23926/EricArcher/rfPermute)  
[![Travis-CI Build Status](https://travis-ci.org/EricArcher/rfPermute.svg?branch=master)](https://travis-ci.org/EricArcher/rfPermute)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/EricArcher/rfPermute?branch=master&svg=true)](https://ci.appveyor.com/project/EricArcher/rfPermute)

# rfPermute

## Description

`rfPermute` estimates the significance of importance metrics for a Random Forest model by permuting the response variable. It will produce null distributions of importance metrics for each predictor variable and p-value of observed. The package also includes several summary and visualization functions for `randomForest` and `rfPermute` results.

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

## Current Functions

`classConfInt` Classification Confidence Intervals  
`cleanRFdata` Clean Random Forest Input Data  
`confusionMatrix` Confusion Matrix  
`exptdErrRate` Expected Error Rate  
`impHeatmap` Importance Heatmap  
`pctCorrect` Percent Correctly Classified  
`plotConfMat` Heatmap representation of Confusion Matrix  
`plotImpVarDist` Distribution of Important Variables  
`plotInbag` Distribution of sample inbag rates  
`plotNull` Plot Random Forest Importance Null Distributions  
`plotOOBtimes` Distribution of sample OOB rates  
`plotPredictedProbs` Distribution of prediction assignment probabilities  
`plotRFtrace` Trace of cumulative error rates in forest  
`plotVotes` Vote Distribution  
`plot.rp.importance` Plot Random Forest Importance Distributions  
`proximityPlot` Plot Random Forest Proximity Scores  
`rfPermute` Estimate Permutation p-values for Random Forest Importance Metrics  
`rp.combine` Combine rfPermute Objects  
`rp.importance` Extract rfPermute Importance Scores and p-values  

## version 2.1.7 (devel)

* Fixed bug in parallel processing code.  

## version 2.1.6 (on CRAN)

* Added `plotConfMat`, `plotOOBtimes`, `plotRFtrace`, and `plotInbag`, and `plotImpVarDist` visualizations.  
* Changed `confusionMatrix` so it will work when `randomForest` model doesn't have a `$confusion` element, like when model is result of `combine`-ing multiple models.   
* Improved efficiency and stability of parallel processing code. Changed default value of `num.cores` to `NULL`.  

## version 2.1.5

* Added `type` argument to `plotVotes` to choose between area and bar charts.
* Changed `plot.rfPermute` to `plotNull` to avoid clashes and maintain functionality of `randomForest::plot.randomForest`.
* Changed name of `proximity.plot` to `proximityPlot`,  `exptd.err.rate` to `exptdErrRate`, and `clean.rf.data` to `cleanRFdata` to make camelCase naming scheme more consistent in package.
* Changed `plotNull` from base graphics to *ggplot2*.
* Added `symb.metab` data set.

## version 2.1.1

* Added `n` argument to `impHeatmap`.
* Added functions: `classConfInt`, `confusionMatrix`, `plotVotes`, `pctCorrect`.

## version 2.0.1

* Fixed bug in `plot.rfPermute` that was reporting the p-value incorrectly at the top of the figure.
* Fixed multi-threading in `rfPermute` so it works on Windows too.
* Added `impHeatmap` function.
* Switched `proximity.plot` to use `ggplot2` graphics.

## version 2.0

* Fixed bug with calculation of p-values not respecting importance measure scaling (division by standard deviations). New format of output of `rfPemute` has separate `$null.dist` and `$pval` elements, each with results for unscaled and scaled importance mesures. See `?rfPermute` for more information.
* `rp.importance` and `plot.rfPermute` now take a `scale` argument to specify whether or not importance values should be scaled by standard deviations.
* If `nrep = 0` for `rfPermute`, a `randomForest` object is returned.

## version 1.9.3

* Fixed import declarations to avoid `grid` name clashes.
* Fixed logic error in `clean.rf.data` where fixed predictors were not removed.
* Fixed error in use of `main` argument in `plot.rp.importance`.

## version 1.9.2

* Added this NEWS.md
* Added README.md
* Added `num.cores` argument to `rfPermute` to take advantage of multi-threading 

## version 1.9.1

* Added internal keyword to `calc.imp.pval` to keep it from indexing
* Updated imports to match new CRAN policies