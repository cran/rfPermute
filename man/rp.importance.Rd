\name{rp.importance}
\alias{rp.importance}
\title{Extract rfPermute importance scores and p-values.}
\usage{
  rp.importance(x, sort.by = "MeanDecreaseAccuracy",
    decreasing = T)
}
\arguments{
  \item{x}{An object produced by a call to
  \code{rfPermute}.}

  \item{sort.by}{character vector giving the importance
  metric(s) or p-values to sort by.}

  \item{decreasing}{logical. Should the sort order be
  increasing or decreasing?}
}
\description{
  Extract a matrix of the observed importance scores and
  p-values from the object produced by a call to
  \code{rfPermute}.
}
\details{
  p-values can be given to the \code{sort.by} argument by
  adding '.pval' to the column name of the desired column
  from the \code{importance} element of the
  \code{rfPermute} object
}
\author{
  Eric Archer <eric.archer@noaa.gov>
}

