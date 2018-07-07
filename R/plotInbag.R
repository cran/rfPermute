#' @title Plot inbag distribution
#' @description Plot distribution of sample inbag rates
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param sampsize optional vector of sample sizes used in \code{rf} model.
#' @param bins number of bins in histogram.
#' @param plot display the plot?
#'   
#' @return the \code{ggplot2} object is invisibly returned. The red vertical 
#'   lines mark the expected values for the classes in the model based on their 
#'   frequency and sample sizes.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' plotInbag(rf)
#' 
#' @importFrom magrittr %>% 
#' @export
#'
plotInbag <- function(rf, sampsize = NULL, bins = 20, plot = TRUE) {
  p <- data.frame(inbag = 1 - (rf$oob.times / rf$ntree)) %>% 
    ggplot2::ggplot(ggplot2::aes_string("inbag")) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::xlab("Percent of trees where sample was inbag")
  if(!is.null(sampsize)) {
    exp.inbag <- as.vector(sampsize / table(rf$y))
    p <- p +
      ggplot2::geom_vline(xintercept = exp.inbag, color = "red")
  }
  
  if(plot) print(p)
  invisible(p)
}