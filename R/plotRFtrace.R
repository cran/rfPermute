#' @title OOB Trace
#' @description Plot trace of cumulative OOB error rate by number of trees
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
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
#' plotRFtrace(rf)
#' 
#' @export
#'
plotRFtrace <- function(rf, plot = TRUE) {
  if(!utils::hasName(rf, "err.rate")) {
    stop(
      "'rf' does not have an 'err.rate' matrix. ",
      "Is it the result of a 'randomForest::combine(...)' operation?"
    )
  }
  
  class.cols <- c("black", scales::hue_pal()(nlevels(rf$y)))
  class.lines <- c(1, rep(2, nlevels(rf$y)))
  names(class.cols) <- names(class.lines) <- c("OOB", levels(rf$y))
  
  df <- as.data.frame(rf$err.rate)
  p <- df %>% 
    dplyr::mutate(trees = 1:dplyr::n()) %>% 
    tidyr::gather("class", "error", -.data$trees) %>% 
    dplyr::mutate(class = factor(.data$class, levels = colnames(df))) %>% 
    ggplot2::ggplot(ggplot2::aes_string("trees", "error", color = "class")) +
    ggplot2::geom_line(ggplot2::aes_string(linetype = "class")) +
    ggplot2::scale_color_manual(values = class.cols) +
    ggplot2::scale_linetype_manual(values = class.lines) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  if(plot) print(p)
  invisible(p)
}