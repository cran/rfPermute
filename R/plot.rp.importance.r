#' @export
#' @import ggplot2 gridExtra grid
#' @usage \S3method{plot}{rp.importance}(x, alpha = 0.05, sig.only = FALSE, n = NULL, main = NULL, ...)
#' 
#' @title Plot Random Forest Importance Distributions.
#' @description Plot the Random Forest importance distributions,
#' with significant p-values as estimated in rfPermute.
#' 
#' @param x An object produced by a call to \code{\link{rp.importance}}.
#' @param alpha Critical alpha to identify "significant" predictors.
#' @param sig.only Plot only the significant (<= \code{alpha}) predictors?
#' @param n Plot \code{n} most important predictors.
#' @param main Main title for plot.
#' @param ... Optional arguments which will be ignored.
#' 
#' @details The function will generate a panel of plots, one for each importance type.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}

plot.rp.importance <- function(x, alpha = 0.05, sig.only = FALSE, n = NULL, main = NULL, ...) {  
  imp.list <- lapply(seq(1, ncol(x), 2), function(i) {
    imp.df <- as.data.frame(x[, c(i, i+1)])
    colnames(imp.df) <- c("imp", "pval")
    imp.df$pred <- rownames(imp.df)
    imp.df$is.sig <- factor(imp.df$pval <= alpha)
    imp.df <- imp.df[order(imp.df$imp), ]
    rownames(imp.df) <- 1:nrow(imp.df)
    imp.df <- imp.df[order(imp.df$imp, decreasing = TRUE), ]
    if(sig.only) imp.df <- imp.df[as.logical(imp.df$is.sig), ]
    if(!is.null(n) & is.numeric(n)) imp.df <- imp.df[1:min(n, nrow(imp.df)), ]
    with(imp.df, ggplot(imp.df, aes(reorder(pred, imp), imp)) + 
      geom_bar(aes(fill = is.sig), stat = "identity") +
      labs(title = colnames(x)[i], x = "Importance", y = "") + coord_flip() +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "red"))
    )
  })
  suppressWarnings(do.call(gridExtra::grid.arrange, c(imp.list, main = main)))
}