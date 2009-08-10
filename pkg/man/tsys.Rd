\name{tsys}

\alias{tradesys}
\alias{print.tsys}
\alias{as.list.tsys}
\alias{"$<-.tsys"}
\alias{as.tradesys}
\alias{as.tradesys.default}

\title{Trading System Object}

\description{

  Create a "tsys" object, a trading system.

}

\usage{

tradesys(datavars, pricemap=c(Mark=datavars[1]), el=FALSE, es=FALSE, xl=FALSE, xs=FALSE,
         delta=1, size.at=as.logical(c(states[1], diff(states))),
         roll.at=FALSE, formulae=NULL, percent=TRUE, entrywins=FALSE,
         entrycond=FALSE)

}

\arguments{

  \item{datavars}{character vector defining the system's data variable names.}
  \item{pricemap}{named character vector defining the mapping between
    price types and data variable names.}
  \item{el}{enter long signal. expression that evaluates to a logical
    vector.}
  \item{es}{enter short signal. expression that evaluates to a logical
    vector.}
  \item{xl}{exit long signal. expression that evaluates to a logical
    vector.}
  \item{xs}{exit short signal. expression that evaluates to a logical
    vector.}
  \item{delta}{numeric vector defining the relationship between changes
    in price and changes in equity.}
  \item{roll.at}{logical vector stating those times at which a roll
    trade is done.}
  \item{size.at}{logical vector stating those times at which equity is
    re-sized to \code{delta}.}
  \item{formulae}{a named list of expressions.}
  \item{percent}{logical. defines \code{delta} as percentage or absolute
    change in price.}
  \item{entrywins,entrycond}{logical. see \code{signalmap}.}

}

\details{

  A 'tsys' object is a complete description of a trading
  system. \code{tradesys} returns its formal arguments as a named list
  with class attribute 'tsys'. The argument values are first checked for
  consistency. A 'tsys' object can be subsetted in the usual list way
  (the 'tsys' class is stripped) and the \code{[.tsys<-} and
  \code{$.tsys<-} assignment methods call \code{tradesys} with the
  modified value. A \code{print} method prints the contents in a
  sensible manner and the \code{as.list} method and \code{as.tradesys}
  generic allow for translation to and from plain lists.

  \code{datavars} is a character vector defining the data variable names
  the 'tsys' object expects to be given in a call to \code{tsys.frame}. 
  
  \code{pricemap} specifies which data variables contain price
  information. There are five categories of price information that need
  to be mapped to data variables:
  \itemize{
    \item Mark
    \item Long
    \item Short
    \item RollLong
    \item RollShort
  }. These are the names of the vector passed to \code{pricemap}. The
  corresponding values are the variable names to which they are
  mapped. The meaning of this mapping is pretty transparent. A long
  entry or short exit is assumed to trade at the price in the
  \emph{Long} variable, a short entry or long exit is assumed to trade
  at the price in the \emph{Short} variable, and unchanged periods are
  valued using the price in the \emph{Mark} variable.
    
  The vector need not specify all five. By default, 'Long' and 'Short'
  are keyed to 'Mark', 'RollLong' to 'Long' and 'RollShort' to
  'Short'. At least 'Mark' must be defined, but a scalar may be passed,
  in which case all five are given that value. An error is raised if
  \code{datavars} does not contain a column identifier that you pass.

  \code{formulae} is optional. If it is non-null, \code{tsys.frame} will

  evaluate in turn each expression contained in the list and right-ward
  cbind to \code{data} the resulting numeric vector using its list name
  as its column name. An error is raised if the evaluations don't return
  a numeric vector of a length that is a multiple of \code{nrow(data)},
  or if the resulting column names of \code{data} are not unique. This
  provides a mechanism for including calculated data columns. Changes to
  the core data using the \code{coredata} or \code{[} assignment methods
  will automatically recalculate these vectors.

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
   
}

\value{

  A tsys object.

}

\examples{
x <- tradesys(c("Bid","Ask","Close"))

## assignment
x$pricemap <- c(Mark="Close", Long="Ask", Short="Bid")
library(TTR)
x[["el"]] <- quote(SMA(Close, 60) >= SMA(Close, 120))
x$es <- quote(SMA(Close, 60) < SMA(Close, 120))

## extraction
x$el
x$es
x[4:8]
x[["el"]]
x$pricemap

## coercion
as.list(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}