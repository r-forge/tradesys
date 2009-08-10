\name{coredata}

\alias{coredata.tsts}
\alias{coredata<-.tsts}

\title{Extracting/Replacing the Core Data of Trading System Time Series}

\description{
  Methods for extracting and replacing the core time series data from
  \code{tsts} objects.
}

\usage{
\method{coredata}{tsts}(x, \dots)
\method{coredata}{tsts}(x) <- value
}

\arguments{
  \item{x}{a \code{tsts} object.}
  \item{value}{numeric vector or matrix.}
  \item{\dots}{ignored.}
}

\details{
  \code{coredata} strips the 'St', 'Equity', and (if there are any)
  calccols columns from the data matrix and returns a zoo matrix. It
  differs from \code{as.zoo.tsts} only by excluding these columns.

  The assignment method calls \code{tsts} with \code{value} passed to
  it. \code{index(value)} should return the desired index, so zoo
  objects are the natural thing to assign. 
}

\value{
  A tsts object.
}

\examples{
##data(spx)
##spx <- window(spx, start="2005-01-01")
##x <- tsts(spx)
##identical(coredata(x), spx)
## coredata(x) <- window(coredata(x), start="2009-05-01")
##x
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}