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
\alias{states}
\alias{states<-}
\alias{phases}
\alias{phases<-}
\alias{pricecols}
\alias{pricecols<-}

\title{Trading System Time Series}

\description{

  Create a "tsts" object, a trading system time series.

}

\usage{

tsts(data, order.by=index(data), states=0, pricecols=1)

}

\arguments{

  \item{data}{numeric vector or matrix.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  \item{states}{numeric. vector of 1s, -1s and 0s}
  \item{pricecols}{character or named list. see below.}

}

\details{
  
  \code{tsts} returns \code{data} with class attribute "tsts", an index
  attribute containing \code{order.by}, and a "tsts" attribute
  containing copies of the remaining arguments. The value passed to
  \code{data} should be a numeric matrix with unique column names. It
  will be coerced via \code{as.matrix}. 
  
  \code{order.by} defines the ordering index of \code{data}. It is a
  vector of unique values and must have a length equal the number of
  rows in \code{data}. The values can be anything so long as they work
  with \code{sort} and artithmetic operations. \code{Date} objects are
  an obvious choice when \code{data} is a daily time
  series. \code{POSIXct} objects are an obvious choice when it is a
  higher resolution series like tick data.

  \code{states} must be a numeric vector consisting of only 1s, -1s and
  0s and its length must be a multiple of the number of rows in
  \code{data}. NA's are not allowed. 
  
  \code{pricecols} specifies via column names or indices which columns
  hold price information. There are five categories of price
  information.
  \itemize{
    \item Mark
    \item Long
    \item Short
    \item RollLong
    \item RollShort
  }
  These are consulted by \code{equity} and \code{trades}, the functions
  that calculate the system's equity curve, period returns and holding
  period returns. So, a long entry or short exit is assumed to trade at
  the price in the \emph{Long} column, a short entry or long exit is
  assumed to trade at the price in the \emph{Short} column, and
  unchanged periods are valued using the price in the \emph{Mark}
  column. This is the mechanism by which slippage assumptions can be
  incorporated. It also allows system performance to be calculated over
  data different from those over which signals are calculated.
    
  By default, the first three are keyed to the first column and the two
  roll columns are keyed to the value of their respective long or short
  columns. If a scalar is passed, all price columns are given this
  value. An error is raised if \code{data} does not contain the column
  identifiers that you pass.

  A call to \code{tsts} returns \code{data} with with class attribute
  "tsts" and columns "St" (the state vector) and "Eq" (the equity
  vector) cbind'ed to the right. It has an attribute "index" containing
  \code{order.by} and a "tsts" attribute, which is a named list
  containing:\itemize{
    \item pricecols
    \item coreattr
    \item delta
    \item roll.at
    \item size.at
    \item percent
    \item columns
    \item signals
    \item entrywins
    \item entrycond
  }
  All but the first two are assigned default values. They can be altered
  later using the signal and equity encoding tools documented
  seperately. 
  
}

\value{

  A tsts object.

}

\seealso{

  EquityEncoding and SignalEncoding

}

\examples{
data(spx)
spx <- window(spx, start="2009-01-01")
x <- tsts(spx)
is.tsts(x)
class(x[1:10, ]) ## row indexing strips tsts attribute
class(x[, 1:2])  ## ... so does column indexing

tsts(matrix(pi))
tsts(matrix(rnorm(100)))

## Basic methods
print(x)
head(x)
tail(x)
start(x)
end(x)

## Coercion methods
as.matrix(x)
as.zoo(x)

## Extractor/assignment functions
index(x) <- index(x) + 1
index(x)
states(x)
states(x) <- c(0,1,1,1,-1) ## length multiple of nrow(x)
phases(x)
phases(x) <- phases(x)
pricecols(x)
pricecols(x) <- "Close"
pricecols(x) <- list(Long="Close", Short="Open")
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}