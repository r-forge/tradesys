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
\alias{phases}
\alias{phases<-}

\title{Trading System Time Series}

\description{

  Create a "tsts" object, a trading system time series.

}

\usage{

tsts(data, order.by=index(data), pricecols=colnames(data)[1], el=FALSE, es=FALSE, 
     xl=FALSE, xs=FALSE, delta=1, size.at=as.logical(c(St[1], diff(St))),
     roll.at=FALSE, formulae=NULL, percent=TRUE, entrywins=FALSE,
     entrycond=FALSE)

}

\arguments{

  \item{data}{numeric vector or matrix.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  \item{pricecols}{character or named list. see below.}
  \item{el,es,xl,xs}{entry and exit signals as expressions. see below.}
  \item{delta}{a vector or expression that evaluates to a
    vector that is numeric and a multiple of \code{nrow(data)} in length.}
  \item{roll.at,size.at}{as with \code{delta} but logical.}
  \item{formulae}{a named list of expressions.}
  \item{percent,entrywins,entrycond}{logical. see below}

}

\details{
  
  \code{tsts} returns \code{data} with class attribute "tsts", an index
  attribute containing \code{order.by}, and a "tstsp" attribute
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

  \code{formulae} is optional and will evaluate in turn each expression
  contained in the list and right-ward cbind to \code{data} the resulting numeric
  vector using its list name as its column name. An error is raised if
  the evaluations don't return a numeric vector of a length that is a
  multiple of \code{nrow(data)}, or if the resulting column names of
  \code{data} are not unique. This provides a mechanism for including
  calculated data columns. Changes to the core data using the
  \code{coredata} or \code{[} assignment methods will automatically
  recalculate these vectors.

  \code{el}, \code{es}, \code{xl}, and \code{xs}  encode the system's
  enterlong, entershort, exitlong and exitshort signals respectively.
  These must be expressions that evaluate to logical vectors that are a
  length that is a multiple of \code{nrow(data)}. These logical vectors
  are used to calculate the system's states using \code{signalmap};
  consult its documentation for details of how this mapping is done. 

  \code{delta} defines the system's leveraging scheme. It must be a
  numeric vector whose length is a multiple of \code{nrow(data)}, or an
  expression that evaluates to such a vector.

  \code{size.at} defines the times when the effect of price
  change on equity is re-sized to delta. By default these evaluate to
  those times when there is a change in state. \code{roll.at} is
  optional and defines the times when a "roll trade" is performed. Both
  are expressions that evaluate to logical vectors that have a length
  that is a multiple of \code{nrow(data)}. \code{percent} is a logical
  value that specifies whether delta is defined over percentage or
  absolute changes in price. All three parameters are used in calculating
  the system's equity vector. Consult documentation of \code{equity} for
  details.

  The eight parameters that can take expression objects are evaluated in
  a special way, using R's \emph{lazy evaluation} scheme, whereby the
  expressions are not evaluated until referred to in the function's
  code. When these expressions are evaluated inside \code{tsts}, the
  (named) data vectors in \code{data} as well as the vector passed to
  \code{order.by} (but named 'index') are included in the evaluation
  frame, so that these expressions can compute on the indexed data
  passed to \code{data} by simply using a vector's
  name. \code{formulae} is evaluated first and its resulting vectors can
  be referred to by the others. Then the entry/exit signals are
  evaluated and the states column "St" can be referred to by the
  remaining three parameters: \code{delta}, \code{size.at} and
  \code{roll.at}.
 
  \code{entrywins} and \code{entrycond} are logical values that redefine
  the meaning of the signal-to-state mapping in certain edge
  cases. Consult the \code{signalmap} documentation for details.
  
  It is not necessary to define the desired system in a single call to
  \code{tsts}. Configuration can be altered later by changing the
  'tstsp' attribute via the \code{tstsp} assignment method. Everytime
  an assignment is made, the system is re-calculated in full via the
  evaluation protocal described above. 
  
}

\value{

  A tsts object.

}

\seealso{

  ConvienienceFunctions, tstsp

}

\examples{
data(spx)
spx <- window(spx, start="2005-01-01")
library(TTR)

x <- tsts(spx, el=SMA(Close, 20) >= SMA(Close, 10),
          es=SMA(Close, 20) < SMA(Close, 10), pricecols="Close")

is.tsts(x)
tail(x)
tail(coredata(x))

class(x[1:10, ]) ## row indexing strips tsts attribute
class(x[, 1:2])  ## ... so does column indexing

## some meaningless edge cases
tsts(pi)
tsts(rnorm(100))

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
states(x)
##states(x) <- 1
##phases(x)
##phases(x) <- phases(x)

tstsp(x)$el <- quote(SMA(Close, 30) >= SMA(Close, 10))
tstsp(x)$entrycond <- TRUE
as.tsts(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}