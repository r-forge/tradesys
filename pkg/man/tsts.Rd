\name{tsts}

\alias{tsts}
\alias{as.matrix.tsts}
\alias{as.zoo.tsts}
\alias{is.tsts}
\alias{head.tsts}
\alias{tail.tsts}
\alias{print.tsts}
\alias{tsys}
\alias{tsys<-}
\alias{tsysvars}
\alias{tsysvars<-}
\alias{[.tsts}

\title{Trading System Time Series}

\description{

  Create a "tsts" object, a trading system time series.

}

\usage{

tsts(data, order.by=index(data), ..., tsys=NULL)

}

\arguments{

  \item{data}{numeric vector or matrix.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  \item{\dots}{args passed to \code{tradesys}.}
  \item{tsys}{object of class 'tsys'.}

}

\details{

  \code{tsts} returns an object of class 'tsts', which subclasses the
  class 'zoo'. It is a mechanism for combining tradesys objects and data
  into a single object. \code{tsts} either takes a tradesys object
  passed to the \code{tsys} parameter, or it constructs one by passing
  \dots to \code{tradesys}. Then \code{tradesys.frame} is called with
  \code{data} and the return coerced into a zoo object. The trading
  system itself is stored in the 'tsys' slot.
  
  The value passed to \code{data} should be a numeric matrix with unique
  column names. It will be coerced via \code{as.matrix}.
  
  \code{order.by} works as it does with ordinary zoo objects in that the
  the values can be anything so long as they are unique and work with
  \code{sort}. Here they should also work with the \eqn{-} operator, as
  the difference between two indices is interpreted as a measure of
  period time. \code{Date} objects are an obvious choice when
  \code{data} is a daily time series. \code{POSIXct} objects are an
  obvious choice when it is a higher resolution series like tick data.
  
  It is not necessary to define the desired system in a single call to
  \code{tsts}. Configuration can be altered later by changing the 'tsys'
  attribute via the \code{tsys} assignment method. Everytime an
  assignment is made, the tradesys expressions are re-evaluated.
  
}

\value{

  A tsts object, which is a subclass of zoo.

}

\examples{
library(TTR)

x <- tsts(spx, prices=quote(Close))

is.tsts(x)

## Basic methods
## print(x) 
head(x)
tail(x)
start(x)
end(x)

## coredata returns what tsts is passed
## !! FIX ME !!
## stopifnot(identical(tail(coredata(x)), tail(spx)))

class(x[1:10, ]) ## row indexing strips tsts attribute
## class(x[, 1:2])  ## ... so does column indexing

## ## Coercion methods
tail(as.matrix(x))
tail(as.zoo(x))

## Extractor/assignment functions
index(x) <- index(x) + 1
tsys(x)
tsys(x)$Ratio <- quote(SMA(prices, 60) / SMA(prices, 120))
tail(x)
coredata(x) <- coredata(x)[, -c(1:3)]
tail(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}