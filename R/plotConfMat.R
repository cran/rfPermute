#' @title Plot Confusion Matrix
#' @description Plot confusion matrix heatmap.
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param title a title for the plot.
#' @param plot display the plot?
#'   
#' @return the \code{ggplot2} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' plotConfMat(rf)
#' 
#' @export
#'
plotConfMat <- function(rf, title = NULL, plot = TRUE) {
  conf <- .confMat(rf)
  pct.correct <- (100 * sum(diag(conf)) / sum(conf)) %>% 
    round(0) %>% 
    paste0("% correct")
  title <- if(is.null(title)) {
    pct.correct 
  } else {
    paste0(title, " (", pct.correct, ")")
  }
  freq <- rowSums(conf)
  rownames(conf) <- paste0(names(freq), " (", freq, ")")
  
  p <- conf %>% 
    prop.table(1) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("observed") %>% 
    tidyr::gather("predicted", "prop", -.data$observed) %>% 
    dplyr::mutate(
      observed = factor(.data$observed),
      observed = stats::reorder(.data$observed, dplyr::desc(.data$observed)),
      predicted = factor(.data$predicted)
    ) %>% 
    ggplot2::ggplot(ggplot2::aes_string("predicted", "observed")) +
    ggplot2::geom_raster(ggplot2::aes_string(fill = "prop")) +
    ggplot2::scale_fill_viridis_c(
      option = "magma", 
      direction = -1, 
      limits = c(0, 1)
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = "Predicted", y = "True", title = title) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title = "Proportion")) +
    ggplot2::theme(
      axis.text.x.top = ggplot2::element_text(angle = 45, hjust = 0),
      panel.background = ggplot2::element_blank()
    )
  
  if(plot) print(p)
  invisible(p)
}