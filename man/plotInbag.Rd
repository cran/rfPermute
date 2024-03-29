% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotInbag.R
\name{plotInbag}
\alias{plotInbag}
\title{Plot Inbag distribution}
\usage{
plotInbag(x, bins = 10, replace = TRUE, sampsize = NULL, plot = TRUE)
}
\arguments{
\item{x}{a \code{rfPermute} or \code{randomForest} model object..}

\item{bins}{number of bins in histogram.}

\item{replace}{was sampling done with or without replacement?}

\item{sampsize}{sizes of samples drawn. Either a single value or vector of 
sample sizes as long as the number of classes.}

\item{plot}{display the plot?}
}
\value{
the \code{ggplot2} object is invisibly returned.
}
\description{
Plot distribution of the fraction of trees that samples were
  inbag in the Random Forest model.
}
\note{
Red vertical lines on the plot denote the expected inbag rate(s). 
  These rates are based on the values of \code{replace} and 
  \code{sampsize} supplied. If not specified, they are set to the 
  \code{\link[randomForest]{randomForest}} defaults. If this is not the 
  same as the arguments used to run the model, there will be a mismatch in 
  the location of these indicator lines and the inbag frequency distribution.
}
\examples{
library(randomForest)
data(mtcars)

sampsize = c(5, 5)

rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 10)
plotInbag(rf)

rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 1000)
plotInbag(rf)

rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 10000)
plotInbag(rf)

}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
