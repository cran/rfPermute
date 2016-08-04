#' @title Confusion Matrix
#' @description Generate a confusion matrix for Random Forest analyses with 
#'   error rates translated into percent correctly classified, and columns for 
#'   confidence intervals and expected classification rates (priors) added.
#' 
#' @param rf a \code{\link[randomForest]{randomForest}} object.
#' @param conf.level confidence level for the \code{\link{binom.test}} confidence interval
#' @param threshold threshold to test observed classification probability against.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @seealso \code{\link{classConfInt}}
#' 
#' @examples
#' data(mtcars)
#'
#' rf <- randomForest(factor(am) ~ ., mtcars, importance = TRUE)
#' confusionMatrix(rf)
#' 
#' @export
#' 
confusionMatrix <- function(rf, conf.level = 0.95, threshold = 0.8) {
  conf <- rf$confusion
  # Strip error rate column
  conf <- conf[, -ncol(conf)]
  # Get confidence intervals
  ci <- classConfInt(rf, conf.level = conf.level, threshold = threshold)
  # Get expected error rate (prior)
  prior <- exptd.err.rate(rf)
  prior <- (1 - prior[c(2:length(prior), 1)]) * 100
  # Add rows and columns
  conf <- rbind(conf, Overall = rep(NA, ncol(conf)))
  cbind(conf, ci * 100, Prior = prior)
}