#' @title Importance Heatmap
#' @description Plot heatmap of importance scores or ranks from a 
#'   classification model
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param n Plot \code{n} most important predictors.
#' @param ranks plot ranks instead of actual importance scores?
#' @param plot print the plot?
#' @param xlab,ylab labels for the x and y axes.
#' @param scale For permutation based measures, should the measures be divided 
#'   their "standard errors"?
#' @param alpha a number specifying the critical alpha for identifying 
#'   predictors with importance scores significantly different from random. 
#'   This parameter is only relevant if \code{rf} is a \code{\link{rfPermute}}
#'   object with p-values. Importance measures with p-values less than alpha 
#'   will be denoted in the heatmap by a black border. If set to \code{NULL}, 
#'   no border is drawn.
#' 
#' @details \code{rf} must be a classification model run with 
#'   \code{importance = TRUE}.
#'   
#' @return the \code{ggplot} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' 
#' # A randomForest model
#' rf <- randomForest(factor(am) ~ ., mtcars, importance = TRUE)
#' importance(rf)
#' impHeatmap(rf, xlab = "Transmission", ylab = "Predictor")
#' 
#' # An rfPermute model with significant predictors identified
#' rp <- rfPermute(factor(am) ~ ., mtcars, nrep = 100, num.cores = 1)
#' impHeatmap(rp, xlab = "Transmission", ylab = "Predictor")
#' 
#' @export
#' 
impHeatmap <- function(rf, n = NULL, ranks = TRUE, plot = TRUE, xlab = NULL, 
                       ylab = NULL, scale = TRUE, alpha = 0.05) {
  if(rf$type != "classification") stop("'rf' must be a classification model")
  classes <- levels(rf$y)
  if(!all(classes %in% colnames(rf$importance))) {
    stop("'rf' must be run with 'importance = TRUE'")
  }
  
  # format importance data.frame
  imp <- data.frame(
    randomForest::importance(rf, scale = scale), 
    check.names = FALSE
  )
  imp.val <- imp$MeanDecreaseAccuracy
  imp$predictor <- names(imp.val) <- rownames(imp)
  if(ranks) for(x in classes) imp[[x]] <- rank(-imp[[x]])
  imp <- imp[, c("predictor", classes)] %>% 
    tidyr::gather("class", "value", -.data$predictor) %>% 
    dplyr::mutate(
      class = factor(.data$class, levels = levels(rf$y)),
      predictor = factor(.data$predictor, levels = names(sort(imp.val)))
    )
  num.preds <- length(levels(imp$predictor))
  n <- if(is.null(n)) length(levels(imp$predictor)) else min(c(n, num.preds))
  imp <- imp[imp$predictor %in% levels(imp$predictor)[(num.preds - n + 1):num.preds], ]
  imp <- droplevels(imp)

  # create plot
  g <- ggplot2::ggplot(imp, ggplot2::aes_string("class", "predictor")) +
    ggplot2::geom_raster(ggplot2::aes_string(fill = "value")) + 
    ggplot2::theme(panel.background = ggplot2::element_blank())
  g <- g + if(ranks) {
    ggplot2::scale_fill_gradient2(
      "Rank", low = "#a50026", mid = "#ffffbf", high = "#313695",
       midpoint = mean(range(imp$value)), 
      guide = ggplot2::guide_colorbar(reverse = TRUE)
    )
  } else {
    ggplot2::scale_fill_gradient2(
      "MeanDecreaseAccuracy", low = "#313695", mid = "#ffffbf", high = "#a50026",
      midpoint = mean(range(imp$value))
    )
  }
  g <- g + if(is.null(xlab)) {
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    ggplot2::xlab(xlab)
  }
  g <- g + if(is.null(ylab)) {
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) 
  } else {
    ggplot2::ylab(ylab)
  }
  
  if(inherits(rf, "rfPermute") & !is.null(rf$pval) & !is.null(alpha))  {
    sc <- ifelse(scale, "scaled", "unscaled")
    sig <- sapply(1:nrow(imp), function(i) {
      pred <- as.character(imp$predictor[i])
      cl <- as.character(imp$class[i])
      rf$pval[pred, cl, sc] <= alpha
    })
    sig.df <- imp[sig, ]
    sig.df$xmin <- as.integer(sig.df$class) - 0.5
    sig.df$xmax <- as.integer(sig.df$class) + 0.5
    sig.df$ymin <- as.integer(sig.df$predictor) - 0.5
    sig.df$ymax <- as.integer(sig.df$predictor) + 0.5
    g <- g + ggplot2::geom_rect(
      ggplot2::aes_string(
        xmin = "xmin", xmax = "xmax", 
        ymin = "ymin", ymax = "ymax"
      ),
      data = sig.df, fill = NA, size = 1, color = "black"
    )
  }
  
  if(plot) print(g)
  invisible(g)
}