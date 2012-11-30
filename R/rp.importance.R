#' Extract a matrix of the observed importance scores
#' and p-values from the object produced by a 
#' call to \code{rfPermute}.
#'
#' @title Extract rfPermute importance scores and p-values.
#' @export rp.importance
#' @usage rp.importance(x, sort.by = "MeanDecreaseAccuracy", decreasing = T)
#' @param x An object produced by a call to \code{rfPermute}.
#' @param sort.by character vector giving the importance metric(s) or p-values to sort by.
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @details p-values can be given to the \code{sort.by} argument by adding '.pval' to the 
#'   column name of the desired column from the \code{importance} element of the \code{rfPermute}
#'   object
#' @author Eric Archer <eric.archer@@noaa.gov>

rp.importance <- function(x, sort.by = "MeanDecreaseAccuracy", decreasing = T) {
  # Extract matrix of importance scores and p-values
  #
  # 'x': an object of class 'rfPermute' with a 'null.dist' element
  # 'sort.by': either a character or numeric vector of the importance metrics to sort by
  # 'decreasing': whether to sort importance scores in decreasing order
  #
  #  11/30/2012
  
  if(!inherits(x, "rfPermute")) stop("'x' is not of class 'rfPermute'")
  if(!is.character(sort.by) & !is.vector(sort.by)) stop("'sort.by' is not a character vector")
  
  imp <- x$importance
  pval <- x$null.dist$pval
  colnames(pval) <- paste(colnames(pval), ".pval", sep = "")
  pred <- rownames(imp)
  vals <- do.call(cbind, lapply(1:ncol(imp), function(i) cbind(imp[, i, drop = F], pval[, i, drop = F])))
  
  not.found <- sort.by[!(sort.by %in% colnames(vals))]
  if(length(not.found) > 0) {
    not.found <- paste(not.found, collapse = ", ")
    stop(paste("sort.by: ", not.found, " not found", sep = ""))
  }
  
  order.list <- lapply(sort.by, function(i) vals[, i])
  order.list <- c(order.list, decreasing = decreasing)
  vals[do.call(order, order.list), ]
}

