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
\alias{is.tsts}
\alias{as.tsts}
\alias{as.tsts.default}
\alias{[.tsts}

\title{Trading System Time Series}

\description{
  Create a "tsts" object, a trading system time series.
}

\usage{
tsts(data, order.by=index(data), states=0, roll.at=NULL,
     pricecols=1, entrywins=FALSE, entrycond=FALSE)
}

\arguments{
  \item{data}{numeric vector or matrix.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  \item{states}{numeric. vector of 1s, -1s and 0s}
  \item{roll.at}{vector of index points points where roll trades are
    done. (Must be the same class as \code{order.by}.)}
  \item{pricecols}{see below.}
  \item{entrywins}{logical. see below.}
  \item{entrycond}{logical. see below.}
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
  hold price information. By default, all price information is in the
  first column. Sometimes more detail is needed and a named list is
  passed. Some examples of how this is done.
  \itemize{
    \item \code{list(enter=2, exit=3)} entry prices in second column, exit in
    third.
    \item \code{list(long="ask", short="bid")} long and short
    trade prices in columns "ask" and "bid" respectively.
    \item \code{list(valuation=2, enterlong=3, entershort=4)} valuation price in
    column 2, long entries in column 3, short entries in column 4.
    \item \code{list(roll=4)} roll prices in column 4.
    \item \code{list(rolllong=4,rollshort=5} roll long prices in column 4, roll
    short prices in column 5.
  }
  Valid list names are: \emph{valuation}, \emph{enter}, \emph{exit},
  \emph{long}, \emph{short}, \emph{roll}, \emph{enterlong},
  \emph{entershort}, \emph{exitlong}, \emph{exitshort}, \emph{rolllong},
  and \emph{rollshort}. Any of these not specified are given the column
  index 1. If a scalar is pased, all price columns are given this
  value. An error is raised if \code{data} does not contain the column
  identifiers that you pass. 
  
  \code{roll.at} contains....
}

\value{
  A tsts object.
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

## Availiable methods
print(x)
head(x)
tail(x)
start(x)
end(x)
as.matrix(x)
as.zoo(x)
index(x) <- index(x) + 1
index(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}