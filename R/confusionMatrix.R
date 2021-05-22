#' @title Confusion Matrix
#' @description Generate a confusion matrix for Random Forest analyses with 
#'   error rates translated into percent correctly classified, and columns for 
#'   confidence intervals and expected classification rates (priors) added.
#' 
#' @param rf a \code{\link[randomForest]{randomForest}} object.
#' @param conf.level confidence level for the \code{\link{binom.test}} confidence interval
#' @param threshold threshold to test observed classification probability against.
#' @param digits integer indicating the number of decimal places to round values 
#'   to. 
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @seealso \code{\link{classConfInt}}, \code{\link{exptdErrRate}}
#' 
#' @examples
#' data(mtcars)
#'
#' rf <- randomForest(factor(am) ~ ., mtcars, importance = TRUE)
#' confusionMatrix(rf)
#' 
#' @export
#' 
confusionMatrix <- function(rf, conf.level = 0.95, threshold = NULL, digits = 1) {
  # Get confusion matrix
  cm <- .confMat(rf)
  cm <- rbind(cm, Overall = rep(NA, ncol(cm)))
  
  # Get confidence intervals
  ci <- classConfInt(rf, conf.level = conf.level, threshold = threshold)
  
  # Get expected error rate (prior)
  prior <- exptdErrRate(rf)
  prior[, "prior"] <- round((1 - prior[, "prior"]) * 100, digits)

  cbind(cm, round(ci * 100, digits), prior)
}
