\name{rfPermute}
\alias{rfPermute}
\alias{rfPermute.default}
\alias{rfPermute.formula}
\title{Estimate permutation p-values for importance metrics.}
\usage{
  rfPermute(x, ...)

  \method{rfPermute}{default}(x, y, \dots, nrep = 1000)

  \method{rfPermute}{formula}(formula, data = NULL, \dots,
    subset, na.action = na.fail, nrep = 100)
}
\arguments{
  \item{x,y,formula,data,subset,na.action,\dots}{See
  \code{\link{randomForest}} for definitions.}

  \item{nrep}{Number of permutation replicates to run to
  construct null distribution and calculate p-values
  (default = 100).}
}
\value{
  An \code{rfPermute} object which contains all of the
  components of a \code{randomForest} object plus:
  \item{null.dist}{A list containing three matrices. The
  first two matrices are null distributions for the
  importance metrics (%IncMSE and IncNodePurity for
  regression models, and MeanDecreaseAccuracy and
  MeanDecreaseGini for classification models) and have
  \code{nrep} rows and one column for each predictor
  variable. The third matrix (\code{pval}) has one row for
  each predictor variable and one column for each
  importance metric. The values are the permutation
  p-values for the respective importance metrics calculated
  as: \eqn{(N(rep >= obs) + 1) / (nrep + 1)}.  }
}
\description{
  Estimate significance of importance metrics for a Random
  Forest model by permuting the response variable.
  Produces null distribution of importance metrics for each
  predictor variable and p-value of observed.
}
\note{
  All other parameters are as defined in
  \code{randomForest.formula}. A Random Forest model is
  first created as normal to calculate the observed values
  of variable importance. \code{rfPermute} then permutes
  the response variable \code{nrep} times, with a new
  Random Forest model built for each permutation step. If
  multiple processors are available, the permutations will
  be distributed amongst them using the package
  \code{multicore}.
}
\examples{
# A regression model using the ozone example
  data(airquality)
  ozone.rfP <- rfPermute(Ozone ~ ., data = airquality, ntree = 500, na.action = na.omit, nrep = 100)
  print(ozone.rfP$importance)  # The original importance metrics.
  print(ozone.rfP$null.dist$pval) # The p-values for each variable.
  plot(ozone.rfP, imp.type = 1) # Plot the null distributions and observed values.
}
\author{
  Eric Archer <eric.archer@noaa.gov>
}
\seealso{
  \code{\link{plot.rfPermute}} for plotting null
  distributions from the \code{rfPermute} object

  package \code{\link{randomForest}}
}
\keyword{classif}
\keyword{regression}
\keyword{tree}

