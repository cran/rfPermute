#' @title Expected Error Rate
#' @description Calculate expected OOB error rates based on random assignment 
#'   and class sizes (prior).
#' 
#' @param rf a \code{link{randomForest}} or \code{\link{rfPermute}} object.
#' 
#' @return a vector of expected error rates.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
exptd.err.rate <- function(rf) {
  if(!inherits(rf, "randomForest")) {
    stop("'rf' is not a randomForest or rfPermute object.")
  }
  y.freq <- table(rf$y)
  exp.err <- 1 - y.freq / sum(y.freq)
  oob.err <- sum(exp.err * y.freq) / sum(y.freq)
  c(OOB = oob.err, exp.err)
}
