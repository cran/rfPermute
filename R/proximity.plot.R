#' @title Plot Random Forest Proximity Scores.
#' @description Create a plot of Random Forest proximity scores using 
#'   multi-dimensional scaling.
#' 
#' @param rf A \code{randomForest} object.
#' @param dim.x,dim.y Numeric values giving x and y dimensions to plot from 
#'   multidimensional scaling of proximity scores.
#' @param legend.loc Character keyword specifying location of legend. 
#'   Can be \code{"bottom", "top", "left", "right"}.
#' @param point.size Size of central points.
#' @param circle.size Size of circles around correctly classified 
#'   points as argument to 'cex'. Set to NULL for no circles.
#' @param circle.border Width of circle border.
#' @param hull.alpha value giving alpha transparency level for convex hull shading. 
#'   Setting to \code{NULL} produces no shading. Ignored for regression models.
#' @param plot logical determining whether or not to show plot.
#' 
#' @details Produces a scatter plot of proximity scores for \code{dim.x} and 
#'   \code{dim.y} dimensions from a multidimensional scale (MDS) conversion of 
#'   proximity scores from a \code{randomForest} object. For classification 
#'   models, a convex hull is drawn around the a-priori classes with points 
#'   colored according to original (inner) and predicted (outer) class.
#'   
#' @return a list with \code{prox.cmd}: the MDS scores of the selected dimensions, 
#'   and \code{g} the \code{\link{ggplot}} object.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' rf <- randomForest(factor(am) ~ ., mtcars, proximity = TRUE)
#' rf
#' proximity.plot(rf)
#' 
#' @importFrom stats cmdscale
#' @importFrom grDevices chull rainbow
#' @importFrom ggplot2 ggplot aes geom_point labs theme element_blank geom_polygon element_rect
#' @export
#' 
proximity.plot <- function(rf, dim.x = 1, dim.y = 2, 
                           legend.loc = c("top", "bottom", "left", "right"),
                           point.size = 2, circle.size = 8, circle.border = 1, 
                           hull.alpha = 0.3, plot = TRUE) {
  
  if(is.null(rf$proximity)) {
    stop("'rf' has no 'proximity' element. rerun with 'proximity = TRUE'")
  }
  
  prox.cmd <- cmdscale(1 - rf$proximity, k = max(c(dim.x, dim.y)))
  prox.cmd <- prox.cmd[, c(dim.x, dim.y)]
  df <- data.frame(prox.cmd, class = rf$y, predicted = rf$predicted)
  colnames(df)[1:2] <- c("x", "y")
  
  g <- ggplot(df, aes_string("x", "y")) 
  
  # Add convex hulls
  if(rf$type != "regression") {
    loc.hull <- tapply(1:nrow(prox.cmd), rf$y, function(i) {
      ch <- chull(prox.cmd[i, 1], prox.cmd[i, 2])
      c(i[ch], i[ch[1]])
    })
    for(ch in 1:length(loc.hull)) {
      ch.df <- df[loc.hull[[ch]], ]
      g <- g + if(!is.null(hull.alpha)) {
        geom_polygon(
          aes_string("x", "y", color = "class", fill = "class"), 
          alpha = hull.alpha,
          data = ch.df, 
          inherit.aes = FALSE, 
          show.legend = FALSE
        ) 
      } else {
        geom_polygon(
          aes_string("x", "y", color = "class"), 
          fill = "transparent",
          data = ch.df, 
          inherit.aes = FALSE, 
          show.legend = FALSE
        ) 
      }
    }
  }
  
  g <- g + geom_point(aes_string(color = "class"), size = point.size) +
    labs(x = paste("Dimension", dim.x), y = paste("Dimension", dim.y)) +
    theme(
      legend.position = match.arg(legend.loc),
      legend.title = element_blank(),
      legend.key = element_rect(color = NA, fill = NA),
      panel.background = element_rect(color = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Add predicted circles
  if(!is.null(circle.size)) {
    g <- g + geom_point(
      aes_string(color = "predicted"), 
      shape = 21, 
      size = circle.size,
      stroke = circle.border,
      show.legend = FALSE
    )
  }
  
  if(plot) print(g)
  invisible(list(prox.cmd = prox.cmd, g = g))
}