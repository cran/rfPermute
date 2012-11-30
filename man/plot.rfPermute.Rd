\name{plot.rfPermute}
\alias{plot.rfPermute}
\title{Plot Random Forest importance null distributions.}
\usage{
  \method{plot}{rfPermute}(x, imp.type, ...)
}
\arguments{
  \item{x}{An object produced by a call to
  \code{rfPermute}.}

  \item{imp.type}{Either a numeric or character vector
  giving the importance metric(s) to plot.}

  \item{...}{Optional graphical arguments to be sent to
  \code{par}.}
}
\description{
  Plot the Random Forest null distributions importance
  metrics, observed values, and p-values for each predictor
  variable from the object produced by a call to
  \code{rfPermute}. ---extra line ---- another line
}
\details{
  The function will generate an individual plot for each
  variable and importance metric on the default graphics
  device.
}
\author{
  Eric Archer <eric.archer@noaa.gov>
}

