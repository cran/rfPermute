#' @title Plot Important Variable Distribution
#' @description Plot distribution of predictor variables on classes sorted 
#'   by order of importance in model.
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param df data.frame with predictors in \code{rf} model.
#' @param class.col response column name in \code{df}.
#' @param max.vars number of variables to plot (from most important to least).
#' @param plot display the plot?
#'   
#' @return the \code{ggplot2} object is invisibly returned.
#' 
#' @note If the model in \code{rf} was run with \code{importance = TRUE}, then
#'   'MeanDecreaseAccuracy' is used as the importance measure. Otherwise,
#'   'MeanDecreaseGini' is used.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' df <- mtcars
#' df$am <- factor(df$am)
#' 
#' rf <- randomForest(am ~ ., df, importance = TRUE)
#' plotImpVarDist(rf, df, "am")
#' 
#' @export
#'
plotImpVarDist <- function(rf, df, class.col, max.vars = 16, plot = TRUE) {
  if(!class.col %in% colnames(df)) {
    stop("'class.col' = \"", class.col, "\" cannot be found in 'df'")
  }
  
  if(ncol(df) <= 2) return(NULL)
  imp.val <- if("MeanDecreaseAccuracy" %in% colnames(rf$importance)) {
    "MeanDecreaseAccuracy" 
  } else {
    "MeanDecreaseGini"
  }
  var.imp <- randomForest::importance(rf)[, imp.val]
  var.imp <- names(sort(var.imp, decreasing = TRUE))
  max.vars <- min(c(length(var.imp), max.vars))
  var.imp <- var.imp[1:max.vars]
  
  df$.class. <- factor(df[[class.col]])
  df <- df[, c(".class.", var.imp)]
  
  p <- df %>%
    tidyr::gather("var", "value", -.data$.class.) %>% 
    dplyr::mutate(var = factor(.data$var, levels = var.imp)) %>% 
    ggplot2::ggplot(ggplot2::aes_string(".class.", "value")) +
    ggplot2::geom_jitter(size = 0.2, alpha = 0.2) +
    ggplot2::geom_violin(alpha = 0.5) +
    ggplot2::facet_wrap(~ var, scales = "free_y") +
    ggplot2::labs(x = "Class", y = "Value") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))  
  
  if(plot) print(p)
  invisible(p)
}