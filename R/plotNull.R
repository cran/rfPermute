#' @title Plot Random Forest Importance Null Distributions
#' @description Plot the Random Forest null distributions importance metrics, 
#' observed values, and p-values for
#' each predictor variable from the object produced by a 
#' call to \code{\link{rfPermute}}.
#' 
#' @param x An object produced by a call to \code{\link{rfPermute}}.
#' @param preds a character vector of predictors to plot. If \code{NULL}, then 
#'   all predictors are plotted.
#' @param imp.type Either a numeric or character vector giving the 
#'   importance metric(s) to plot.
#' @param scale Plot importance measures scaled (divided by) standard errors?
#' @param plot.type type of plot to produce: \code{"density"} for smoothed density 
#'   plot, or \code{"hist"} for histogram.
#' @param plot display the plot?
#' 
#' @details The function will generate an plot for each predictor, with facetted 
#'   importance metrics. The vertical red line shows the observed importance 
#'   score and the p-value is given in the facet label. 
#'   
#' @return A named list of the \code{ggplot} figures produced is invisbly returned.
#'   
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' # A regression model using the ozone example
#' data(airquality)
#' ozone.rfP <- rfPermute(
#'   Ozone ~ ., data = airquality, ntree = 100, 
#'   na.action = na.omit, nrep = 50, num.cores = 1
#' )
#'   
#' # Plot the null distributions and observed values.
#' plotNull(ozone.rfP) 
#' 
#' @importFrom reshape2 melt
#' @export
#' 
plotNull <- function(x, preds = NULL, imp.type = NULL, scale = TRUE, 
                     plot.type = c("density", "hist"), plot = TRUE) {
  
  if(!inherits(x, "rfPermute")) stop("'x' is not of class 'rfPermute'")
  imp <- randomForest::importance(x, type = NULL, class = NULL, scale = scale)
  
  if(is.null(imp.type)) imp.type <- colnames(imp)
  imp.type <- unique(imp.type)
  if(is.character(imp.type)) {
   not.found <- imp.type[!(imp.type %in% colnames(imp))]
   if(length(not.found) > 0) {
     imp <- paste(not.found, collapse = ", ")
     stop(paste("imp.type: ", imp, " is not in 'x'", sep = ""))
   }
  } else if(is.numeric(imp.type)) {
    imp <- imp[, c(ncol(imp) -1, ncol(imp))]
    if(!all(imp.type <= ncol(imp))) stop("some 'imp.type' out of range")
    imp.type <- colnames(imp)[imp.type]
  } else stop("'imp.type' is not a character or numeric vector")
  
  sc <- if(scale) "scaled" else "unscaled"
  
  if(is.null(preds)) preds <- rownames(imp)
  preds.not.found <- setdiff(preds, rownames(imp))
  if(length(preds.not.found) > 0) {
    not.found <- paste(preds.not.found, collapse = ", ")
    stop(paste("The following predictors could not be found:", not.found))
  }
  
  plot.type <- match.arg(plot.type)
  g <- sapply(preds, function(pr) {
    df <- melt(
      sapply(imp.type, function(i) x$null.dist[[sc]][pr, i, ]),
      value.name = "importance",
      varnames = c("rep", "imp.type")
    )
    obs <- melt(
      imp[pr, imp.type, drop = FALSE], 
      value.name = "importance",
      varnames = c("predictor", "imp.type")
    )
    
    pval <- x$pval[pr, imp.type, sc]
    labels <- paste0(names(pval), "\n(p = ", sprintf("%0.3f", pval), ")")
    levels(df$imp.type) <- levels(obs$imp.type) <- labels
    
    p <- ggplot2::ggplot(df, ggplot2::aes_string("importance"))
    p <- p + if(plot.type == "hist") {
      ggplot2::geom_histogram() 
    } else {
      ggplot2::geom_density()
    }
    p <- p + ggplot2::xlab("Importance") + 
      ggplot2::ggtitle(pr) +
      ggplot2::geom_vline(
        ggplot2::aes_string(xintercept = "importance"), 
        color = "red", data = obs
      ) +
      ggplot2::facet_wrap(~imp.type, scales = "free")
      
    return(p)
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  if(plot) for(p in g) print(p)
  invisible(g)
}