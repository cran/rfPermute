#' @title Vote Distribution
#' @description Plot distribution of votes for each sample in each class.
#' 
#' @param rf an object inheriting from \code{\link{randomForest}}.
#' @param plot print the plot?
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
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes_string geom_area scale_fill_discrete guide_legend facet_wrap labs theme element_text element_blank
#' @export
#'
plotVotes <- function(rf, plot = TRUE) {
  votes <- data.frame(y = rf$y, rf$votes)
  vote.order <- do.call(order, c(as.list(votes), list(decreasing = TRUE)))
  votes <- votes[vote.order, ]
  votes$id <- 1:nrow(votes)
  votes <- melt(votes, id.vars = c("id", "y"),
                variable.name = "pred", value.name = "votes")
  
  freq <- table(rf$y)
  levels(votes$y) <- paste(names(freq), " (n = ", freq, ")", sep = "")
  p <- ggplot(votes, aes_string("id", "votes")) +
    geom_area(aes_string(fill = "pred"), stat = "identity") +
    scale_fill_discrete(guide = guide_legend(title = "Predicted")) +
    facet_wrap(~ y, scales = "free_x") +
    labs(x = "", y = "Votes") +
    theme(
      legend.position = "top",
      text = element_text(size = 14),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0)
    )
  if(plot) print(p)
  invisible(p)
}