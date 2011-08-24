#' Plot the Random Forest null distributions importance metrics, 
#' observed values, and p-values for
#' each predictor variable from the object produced by a 
#' call to \code{rfPermute}.
#'
#' @title Plot Random Forest importance null distributions.
#' @export plot.rfPermute
#' @S3method plot rfPermute
#' @param x An object produced by a call to \code{rfPermute}.
#' @param ... Optional graphical arguments to be sent to \code{par}.
#' @note The function will generate an individual plot for
#'   each variable and importance metric on the default graphics
#'   device.
#' @author Eric Archer <eric.archer@@noaa.gov>

plot.rfPermute <- function(x, ...) {
  # Creates density plots of the null distributions for each predictor
  #   and both importance types.  A vertical line marks the observed value. 
  #
  # 'x': an object of class 'rfPermute' with a 'null.dist' element
  #
  #  8/3/2011

  if(!inherits(x, "rfPermute")) stop("'x' is not of class 'rfPermute'")
  importance <- x$importance[, c(ncol(x$importance) - 1, ncol(x$importance))]
  op <- par(..., no.readonly = TRUE)
  for(pred in rownames(x$null.dist$pval)) {
    for(imp.type in 1:2) {
      n <- x$null.dist[[imp.type]][, pred]
      o <- importance[pred, imp.type]
      xlab <- names(x$null.dist)[imp.type]
      pval <- x$null.dist$pval[pred, imp.type]
      main <- c(paste("Variable:", pred), paste("P(null >= obs) =", sprintf("%0.3f", pval)))
      plot(density(n), xlim = range(c(n, o)), xlab = xlab, main = main)
      abline(v = o, lwd = 2)
    }
  }
  par(op)
}

