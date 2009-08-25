\name{tsts}

\alias{tsts}
\alias{print.tsts}
\alias{index.tsts}
\alias{index<-.tsts} 
\alias{start.tsts}
\alias{end.tsts}
\alias{head.tsts}
\alias{tail.tsts}
\alias{as.matrix.tsts}
\alias{as.zoo.tsts}
\alias{[.tsts}
\alias{is.tsts}
\alias{tsys}
\alias{tsys<-}

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
  
  \code{tsts} returns \code{data} with class attribute "tsts", an index
  attribute containing \code{order.by}, and a "tsys" attribute
  containing a "tsys" object constructed from a call to
  \code{tradesys(datacols=datacols, ...)}. The value passed to
  \code{data} should be a numeric matrix with unique column names. It
  will be coerced via \code{as.matrix}.
  
  \code{order.by} defines the ordering index of \code{data}. It is a
  vector of unique values and must have a length equal the number of
  rows in \code{data}. The values can be anything so long as they work
  with \code{sort} and artithmetic operations. \code{Date} objects are
  an obvious choice when \code{data} is a daily time
  series. \code{POSIXct} objects are an obvious choice when it is a
  higher resolution series like tick data.

  \dots are \code{tradesys} parameters. At first a trading system is
  constructed, .... NEED TO COMPLETE.
  
  It is not necessary to define the desired system in a single call to
  \code{tsts}. Configuration can be altered later by changing the
  'tsys' attribute via the \code{tsys} assignment method. Everytime
  an assignment is made, the system is re-calculated in full.
  
}

\value{

  A tsts object.

}

% \seealso{
% }

\examples{
##spx <- window(spx, start="2005-01-01")
library(TTR)

x <- tsts(spx, el=SMA(Close, 20) >= SMA(Close, 10),
          es=SMA(Close, 20) < SMA(Close, 10), pricemap="Close")

is.tsts(x)
tail(x)
tail(coredata(x))

class(x[1:10, ]) ## row indexing strips tsts attribute
class(x[, 1:2])  ## ... so does column indexing

## Basic methods
## print(x) 
head(x)
tail(x)
start(x)
end(x)

## Coercion methods
tail(as.matrix(x))
tail(as.zoo(x))

## Extractor/assignment functions
index(x) <- index(x) + 1
tail(index(x))
tsys(x)
tsys(x)$el <- quote(SMA(Close, 60) >= SMA(Close, 120))
tsys(x)$es <- quote(SMA(Close, 60) <  SMA(Close, 120))
tail(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}