#' @title Clean Random Forest Input Data
#' @description Removes cases with missing data and predictors that are constant.
#' 
#' @param x columns used as predictor variables as character or numeric vector.
#' @param y column used as response variable as character or numeric.
#' @param data data.frame containing 'x' and 'y' columns.
#' @param max.levels maximum number of levels in response variable 'y'.
#' 
#' @return a data.frame containing cleaned data.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom stats complete.cases
#' @export
#' 
clean.rf.data <- function(x, y, data, max.levels = 30) {
  x <- setdiff(x, y)
  sub.df <- data[, c(y, x)]
  sub.df <- sub.df[complete.cases(sub.df), , drop = TRUE]
  
  delete.pred <- character(0)
  for (pred in x) {
    pred.vec <- sub.df[[pred]]
    if (length(unique(pred.vec)) == 0) delete.pred <- c(delete.pred, pred)
    if (is.factor(pred.vec) & (nlevels(pred.vec) > max.levels)) delete.pred <- c(delete.pred, pred)
  }
  delete.pred <- unique(delete.pred)
  if (length(delete.pred) > 0) x <- setdiff(x, delete.pred)

  if (is.factor(sub.df[[y]]) & nlevels(sub.df[[y]][, drop = TRUE]) < 2) return(NULL)
  sub.df[, c(y, x)]
}
