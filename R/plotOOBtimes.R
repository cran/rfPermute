#' @title Plot Times OOB
#' @description Plot histogram of times samples were OOB.
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param bins number of bins in histogram. Defaults to number of samples / 5.
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
#' plotOOBtimes(rf)
#' 
#' @export
#'
plotOOBtimes <- function(rf, bins = NULL, plot = TRUE) {
  if(is.null(bins)) bins <- min(3, floor(length(rf$oob.times) / 5))
  p <- data.frame(times = rf$oob.times) %>% 
    ggplot2::ggplot(ggplot2::aes_string("times")) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::labs(x = "Times OOB", y = "Frequency") 
  
  if(plot) print(p)
  invisible(p)
}