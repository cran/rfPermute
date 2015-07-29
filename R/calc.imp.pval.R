#' @title Calculate Importance p-value
#' @description Calculate p-values for an importance metric from an rfPermute object. Used internally.
#' 
#' @param rp an \code{\link{rfPermute}} object with a \code{null.dist} element.
#' @param imp.names vector of column names of an importance metric from the 
#'   \code{importance} and \code{null.dist} elements.
#' 
#' @return a vector of p-values for each predictor
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @keywords internal
#' 
calc.imp.pval <- function(rp, imp.names) {
  sapply(imp.names, function(i) {
    sapply(rownames(rp$importance), function(pred) {
      num.perm.gte <- sum(rp$null.dist[[i]][, pred] >= rp$importance[pred, i]) + 1
      num.perm.gte / (nrow(rp$null.dist[[i]]) + 1)
    })
  })
}