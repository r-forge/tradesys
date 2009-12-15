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

  \code{coredata} strips the non-core columns from the data
  matrix and returns a zoo matrix. It differs from \code{as.zoo.tsts}
  only by excluding these columns.

  The assignment method calls \code{tsts(value, index(value),
    tsys=tsys(x))}.
  
}

\value{
  
  A tsts object.
  
}

\examples{
x <- tsts(spx, prices=quote(Close))
## !! FIX ME !!
## stopifnot(identical(coredata(x), spx))
coredata(x) <- window(coredata(x), start="2009-05-01")
x
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}