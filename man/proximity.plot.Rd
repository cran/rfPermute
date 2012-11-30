\name{proximity.plot}
\alias{proximity.plot}
\title{Plot proximity scores from a Random Forest object.}
\usage{
  proximity.plot(rf, dim.x = 1, dim.y = 2, legend.loc =
    NULL, grp.cols = NULL, circle.size = 4)
}
\arguments{
  \item{rf}{A \code{randomForest} object.}

  \item{dim.x,dim.y}{Numeric values giving x and y
  dimensions to plot from multidimensional scaling of
  proximity scores.}

  \item{legend.loc}{Character keyword specifying location
  of legend. See \link{legend}.}

  \item{grp.cols}{Character vector specifying colors for
  classes.}

  \item{circle.size}{Size of circles around correctly
  classified points as argument to 'cex'. Set to NULL for
  no circles.}
}
\description{
  Plot proximity scores from a Random Forest object
}
\details{
  Produces a scatter plot of proximity scores for
  \code{dim.x} and \code{dim.y} dimensions from a
  multidimensional scale (MDS) conversion of proximity
  scores from a \code{randomForest} object. A convex hull
  is drawn around the a-priori classes.
}
\author{
  Eric Archer <eric.archer@noaa.gov>
}

