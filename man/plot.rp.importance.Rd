\name{plot.rp.importance}
\alias{plot.rp.importance}
\title{Plot Random Forest importance distributions.}
\usage{
  \method{plot}{rp.importance}(x, alpha = 0.05, sig.only =
    FALSE, n = NULL, main = NULL, ...)
}
\arguments{
  \item{x}{An object produced by a call to
  \code{rp.importance}.}

  \item{alpha}{Critical alpha to identify "significant"
  predictors.}

  \item{sig.only}{Plot only the significant (<=
  \code{alpha}) predictors?}

  \item{n}{Plot \code{n} most important predictors.}

  \item{main}{Main title for plot.}

  \item{...}{Optional arguments which will be ignored.}
}
\description{
  Plot the Random Forest importance distributions, with
  significant p-values as estimated in rfPermute.
}
\details{
  The function will generate a panel of plots, one for each
  importance type.
}
\author{
  Eric Archer <eric.archer@noaa.gov>
}

