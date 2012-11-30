\name{clean.rf.data}
\alias{clean.rf.data}
\title{Clean RandomForest input data}
\usage{
  clean.rf.data(x, y, data, max.levels = 30)
}
\arguments{
  \item{x}{columns used as predictor variables as character
  or numeric vector}

  \item{y}{column used as response variable as character or
  numeric}

  \item{data}{data.frame containing 'x' and 'y' columns}

  \item{max.levels}{maximum number of levels in response
  variable 'y'}
}
\value{
  a data.frame containing cleaned data
}
\description{
  Clean RandomForest input data
}
\author{
  Eric Archer <eric.archer@noaa.gov>
}

