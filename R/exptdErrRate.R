#' @title Expected Error Rate
#' @alias prior, priors
#' @description Calculate expected OOB error rates (priors) for randomForest 
#'   classification model based on random assignment and class sizes.
#' 
#' @param rf an object inheriting from \code{link{randomForest}}.
#' 
#' @return a matrix of expected error rates (\code{prior}) and 
#'   p-values from a binomial test (\code{class_p.value}) for each class.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples 
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' exptdErrRate(rf)
#' 
#' @export
#' 
exptdErrRate <- function(rf) {
  if(!inherits(rf, "randomForest")) {
    stop("'rf' is not a randomForest or rfPermute object.")
  }
  if(rf$type == "regression") stop("'rf' must be of a classification model")
  
  # Get priors
  cm <- .confMat(rf)
  class.n <- colSums(cm)
  total.n <- sum(class.n)
  exp.err <- 1 - class.n / total.n
  prior <- c(exp.err, OOB = sum(exp.err * class.n) / total.n)
  
  # Get p-values
  correct.n <- c(diag(cm), sum(diag(cm)))
  total.n <- c(class.n, total.n)
  prior.p <- 1 - pbinom(correct.n, total.n, 1 - prior)
  
  cbind(prior = prior, class_p.value = prior.p)
}
