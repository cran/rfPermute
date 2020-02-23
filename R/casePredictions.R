#' @title Case Predictions
#' @description Get data frame of case predictions for training data along with 
#'   vote distributions.
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#'   
#' @return A data frame containing columns of original and predicted cases,
#'   whether they were correctly classified, and vote distributions.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' casePredictions(rf)
#' 
#' @export
#'
casePredictions <- function(rf) {    
  if(!inherits(rf, "randomForest")) {
    stop("'rf' must be a randomForest object or inherit from one")
  }
  if(rf$type != "classification") stop("'rf' must be a classification model")

  df <- cbind(
    data.frame(
      case.id = names(rf$y),
      original = rf$y,
      predicted = rf$predicted,
      is.correct = rf$y == rf$predicted
    ),
    rf$votes
  )
  rownames(df) <- NULL
  df
}