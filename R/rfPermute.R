#' Estimate significance of importance metrics for
#' a Random Forest model by permuting the response
#' variable.  Produces null distribution of importance
#' metrics for each predictor variable and p-value of
#' observed.
#'
#' @rdname rfPermute
#' @title Estimate permutation p-values for importance metrics.
#' @aliases rfPermute rfPermute.default rfPermute.formula
#'
#' @export rfPermute
#' @S3method rfPermute default
#' @S3method rfPermute formula
#' 
#' @param x,y,formula,data,subset,na.action,\dots See \code{link{randomForest}} for definitions.
#' @param nrep Number of permutation replicates to run to construct 
#'   null distribution and calculate p-values (default = 100).
#' @param clust.opts List of options for setting up clusters to be passed
#'   to the \code{make.Cluster} function in the \code{parallel} package if multiple 
#'   processors are available. If not specified or \code{NULL} then \code{parallel} is not used.
#'
#' @note All other parameters are as defined in \code{randomForest.formula}. A Random Forest model is
#'   first created as normal to calculate the observed values of variable importance. \code{rfPermute}
#'   then permutes the response variable 'nrep' times, with a new Random Forest model built 
#'   for each permutation step. If multiple processors are available and the
#'   job can be distributed amongst them using the package \code{parallel}, the cluster configuration
#'   can be specified with the \code{clust.opt} argument.  If the clusters
#'   cannot be allocated, then \code{rfPermute} will operate as normal on a single core.
#'
#' @return An \code{rfPermute} object which contains all of the components of a 
#'   \code{randomForest} object plus:
#'   \item{null.dist}{A list containing three matrices. The first two matrices are null distributions
#'     for the importance metrics (%IncMSE and IncNodePurity for regression models, and 
#'     MeanDecreaseAccuracy and MeanDecreaseGini for classification models) and have 
#'     \code{nrep} rows and one column for each predictor variable. The third matrix (\code{pval})
#'     has one row for each predictor variable and one column for each importance metric. The values
#'     are the permutation p-values for the respective importance metrics calculated as: \eqn{(N(rep >= obs) + 1) / (nrep + 1)}.
#'   }
#'
#' @author Eric Archer <eric.archer@@noaa.gov>
#' @keywords tree classif regression
#' @seealso \code{\link{plot.rfPermute}} for plotting null distributions from the \code{rfPermute} object
#' 
#'  package \code{\link{randomForest}} 
#'
#' @examples
#'   # A regression model using the ozone example
#'   data(airquality)
#'   ozone.rfP <- rfPermute(Ozone ~ ., data = airquality, ntree = 500, na.action = na.omit, nrep = 100)
#'   print(ozone.rfP$importance)  # The original importance metrics.
#'   print(ozone.rfP$null.dist$pval) # The p-values for each variable.
#'   plot(ozone.rfP, imp.type = 1) # Plot the null distributions and observed values.
#'
#'   \dontrun{
#'     # A classification model with random dataset 
#'     # using two cores with the \code{parallel} package
#'     set.seed(17)
#'     x <- matrix(runif(500), 100)
#'     y <- gl(2, 50, labels = LETTERS[1:2])
#'     ran.rfP <- rfPermute(x, y, ntree = 500, nrep = 100, clust.opts = 2)
#'     print(ran.rfP$null.dist$pval)
#'   }

rfPermute <- function(x, ...) UseMethod("rfPermute")
