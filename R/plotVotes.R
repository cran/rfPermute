#' @title Vote Distribution
#' @description Plot distribution of votes for each sample in each class.
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param type either \code{area} for stacked continuous area plot or 
#'   \code{bar} for discrete stacked bar chart. The latter is prefered for small 
#'   numbers of cases. If not specified, a bar chart will be used if all 
#'   classes have <= 30 cases.
#' @param plot display the plot?
#'   
#' @return the \code{ggplot} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' plotVotes(rf)
#' 
#' @importFrom swfscMisc plotAssignments
#' @export
#'
plotVotes <- function(rf, type = NULL, plot = TRUE) {  
  if(rf$type != "classification") stop("'rf' must be a classification model")
  plotAssignments(rf$votes, rf$y, type = type, ylab = "Votes", plot = plot)
}