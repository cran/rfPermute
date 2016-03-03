# rfPermute NEWS.md

To install the latest version from GitHub:

```r
# make sure you have Rtools installed
if (!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('EricArcher/rfPermute/rfPermute')
```

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