#' @title Plot Vote Distribution
#' @description Plot distribution of votes for each sample in each class.
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param type either \code{area} for stacked continuous area plot or 
#'   \code{bar} for discrete stacked bar chart. The latter is prefered for small 
#'   numbers of cases. If not specified, a bar chart will be used if all 
#'   classes have <= 30 cases.
#' @param freq.sep.line put frequency of original group on second line in facet 
#'   label? If \code{FALSE}, labels are single line. If \code{NULL} frequencies 
#'   will not be included in labels.
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
#' plotVotes(rf)
#' 
#' @export
#'
plotVotes <- function(rf, type = NULL, freq.sep.line = TRUE, plot = TRUE) {  
  if(rf$type != "classification") stop("'rf' must be a classification model")
  swfscMisc::plotAssignments(
    rf$votes, rf$y, 
    type = type, 
    ylab = "Proportion of votes", 
    freq.sep.line = freq.sep.line,
    plot = plot
  )
}