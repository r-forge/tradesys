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

tsts(data, order.by=index(data), states=NULL, pricecols=1, exprcols, signals, 
     delta=statechg(St), size.at, roll.at, percent=TRUE, entrywins=FALSE, entrycond=FALSE)

}

\arguments{

  \item{data}{numeric vector or matrix.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  \item{states}{numeric. vector of 1s, -1s and 0s.}
  \item{pricecols}{character or named list. see below.}
  \item{exprcols}{a named list of expressions.}
  \item{signals}{a list of up to four expressions named 'el','es','xl'
    and 'xs'}
  \item{delta}{a vector or expression that evaluates to a
    vector that is numeric and a multiple of \code{nrow(data)} in length.}
  \item{roll.at,size.at}{a vector or expression that evaluates to a
    vector that is logical and a multiple of \code{nrow(data)} in
    length.}
  \item{percent,entrywins,entrycond}{logical. see below}

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
  \code{data}. NA's are not allowed. This is a manual mechanism for
  defining the system's states; the algorithmic mechanism is provided by
  \code{signals}, documented below. 
  
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

  \code{exprcols} is optional and will evaluate in turn each expression
  contained in the list and right-ward cbind to \code{data} the resulting numeric
  vector using its list name as its column name. An error is raised if
  the evaluations don't return a numeric vector of a length that is a
  multiple of \code{nrow(data)}, or if the resulting column names of
  \code{data} are not unique. This provides a mechanism for including
  calculated data columns. Changes to the core data using the
  \code{coredata} or \code{[} assignment methods will automatically
  recalculate these vectors.

  \code{signals} will evaluate its up to four expressions and calculate
  the system's states from these using \code{signalmap}; consult its
  documentation for details of how this mapping is done. It is passed a
  named list and the list names must be 'el' (enter long), 'es' (enter
  short), 'xl' (exit long) and 'xs' (exit short). Each expression must
  evaluate to a logical vector that has a length that is a multiple of
  \code{nrow(data)}. These are basically row-wise logical statements
  that specify the system's entry and exit rules. If a non-null value is
  passed to \code{states} along with \code{signals}, the former defines
  the system states and the latter is ignored. However, any subsequent
  change to the 'tsts' object will trigger a recalculation of these
  states using the signals and the initial values will be overwritten. 

  \code{delta} defines the system's leveraging scheme. It must be a
  numeric vector whose length is a multiple of \code{nrow(data)}, or an
  expression that evaluates to such a vector.

  \code{size.at} defines the times when the effect of price
  change on equity is re-sized to delta. By default these evaluate to
  those times when there is a change in state. \code{roll.at} is
  optional and defines the times when a "roll trade" is performed. Both
  are expressions that evaluate to logical vectors that have a length
  that is a multiple of \code{nrow(data)}. \code{percent} is a logical
  value that specifies whether delta is difined over percentage or
  absolute changes in price. All three parameters are used in calculating
  the system's equity vector. Consult documentation of \code{equity} for
  details.

  The four parameters taking expression objects are evaluated in a
  special way, using R's \emph{lazy evaluation} scheme, whereby the
  expressions are not evaluated until referred to in the function's
  code. When these expressions are evaluated inside \code{tsts}, the
  (named) data vectors in \code{data} as well as the vector passed to
  \code{order.by} (but named 'index') are included in the evaluation
  frame, so that these expressions can compute on the indexed data
  passed to \code{data} by simply using a vector's
  name. \code{exprcols} is evaluated first and the resulting vectors can
  be referred to by the other three.
 
  \code{entrywins} and \code{entrycond} are logical values that redefine
  the meaning of the signal-to-state mapping in certain edge
  cases. Consult the \code{signalmap} documentation for details.
  
  A call to \code{tsts} returns \code{data} with with class attribute
  "tsts" and the evaluated exprcols columns and columns "St" (the state
  vector) and "Equity" (the equity vector) cbind'ed to the right. It has
  an attribute "index" containing \code{order.by} and a 'tstsp'
  attribute, which is a named list containing the passed parameters.

  It is not necessary to define the desired system in a single call to
  \code{tsts}. Configuration can be altered later by changing the
  'tstsp' attribute via the \code{tstsp} assignment method. The system's
  signals can be defined/altered at a later date using the assignment
  convienience function \code{signals}.
  
}

\value{

  A tsts object.

}

\seealso{

  ConvienienceFunctions, tstsp, signals

}

\examples{
data(spx)
spx <- window(spx, start="2009-01-01")
x <- tsts(spx, states=0)
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
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}