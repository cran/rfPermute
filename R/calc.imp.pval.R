#' @name calc.imp.pval
#' 
#' @title Calculate Importance p-value
#' @description Calculate p-values for an importance metric from an rfPermute object. Used internally.
#' 
#' @param rp an \code{\link{rfPermute}} object with a \code{null.dist} element.
#' @param imp.type column name of an importance metric from the \code{importance} and \code{null.dist} elements.
#' 
#' @return a vector of p-values for each predictor
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}

calc.imp.pval <- function(rp, imp.type) {
  sapply(rownames(rp$importance), function(pred) {
    num.perm.gte <- sum(rp$null.dist[[imp.type]][, pred] >= rp$importance[pred, imp.type]) + 1
    num.perm.gte / (nrow(rp$null.dist[[imp.type]]) + 1)
  })
}