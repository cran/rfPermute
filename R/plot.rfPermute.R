#' @title Plot Random Forest Importance Null Distributions.
#' @description Plot the Random Forest null distributions importance metrics, 
#' observed values, and p-values for
#' each predictor variable from the object produced by a 
#' call to \code{\link{rfPermute}}.
#' 
#' @param x An object produced by a call to \code{\link{rfPermute}}.
#' @param imp.type Either a numeric or character vector giving the 
#'   importance metric(s) to plot.
#' @param ... Optional graphical arguments to be sent to \code{\link[graphics]{par}}.
#' @details The function will generate an individual plot for
#'   each variable and importance metric on the default graphics
#'   device.
#'   
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export plot.rfPermute
#' @export
plot.rfPermute <- function(x, imp.type = 1, ...) {
  if(!inherits(x, "rfPermute")) stop("'x' is not of class 'rfPermute'")
  if(is.character(imp.type)) {
   not.found <- imp.type[!(imp.type %in% names(x$null.dist))]
   if(length(not.found) > 0) {
     imp <- paste(not.found, collapse = ", ")
     stop(paste("imp.type: ", imp, " not in rfPermute object 'x'", sep = ""))
    }
  } else if(is.numeric(imp.type)) {
    if(!all(imp.type <= ncol(x$importance))) stop("some 'imp.type' out of range")
  } else stop("'imp.type' is not a character or numeric vector")
  
  importance <- x$importance[, c(ncol(x$importance) - 1, ncol(x$importance))]
  op <- par(..., no.readonly = TRUE)
  for(pred in rownames(importance)) {
    for(imp in imp.type) {
      n <- x$null.dist[[imp]][, pred]
      o <- importance[pred, imp]
      xlab <- ifelse(is.character(imp), imp, names(x$null.dist)[imp])
      pval <- x$null.dist$pval[pred, imp]
      main <- c(paste("Variable:", pred), paste("P(null >= obs) =", sprintf("%0.3f", pval)))
      plot(density(n), xlim = range(c(n, o)), xlab = xlab, main = main)
      abline(v = o, lwd = 2)
    }
  }
  par(op)
}

