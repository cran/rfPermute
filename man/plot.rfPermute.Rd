\name{plot.rfPermute}
\alias{plot.rfPermute}
\title{Plot Random Forest importance null distributions.}
\usage{
  plot.rfPermute(x, ...)
}
\arguments{
  \item{x}{An object produced by a call to
  \code{rfPermute}.}

  \item{...}{Optional graphical arguments to be sent to
  \code{par}.}
}
\description{
  Plot the Random Forest null distributions importance
  metrics, observed values, and p-values for each predictor
  variable from the object produced by a call to
  \code{rfPermute}.
}
\note{
  The function will generate an individual plot for each
  variable and importance metric on the default graphics
  device.
}
\author{
  Eric Archer <eric.archer@noaa.gov>
}

