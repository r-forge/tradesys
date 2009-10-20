\name{tradesys}

\alias{tradesys}
\alias{print.tradesys}
\alias{as.list.tradesys}
\alias{$<-.tradesys}
\alias{[[<-.tradesys}
\alias{[<-.tradesys}
\alias{as.tradesys}
\alias{as.tradesys.default}
\alias{tradesys.frame}

\title{Trading System Definition and Evaluation}

\description{

  Use \code{tradesys} to create a "tradesys" object. Use
  \code{tradesys.frame} to evaluate such an object with some dataset. 

}

\usage{

tradesys(states=0, size.at=FALSE, roll.at=FALSE, delta=1, prices, 
         equity=ror(prices, states, roll.at, delta, size.at), ...)

tradesys.frame(x, data, order.by=index(data))

}

\arguments{
  

  \item{x}{a tradesys object.}

  \item{data}{matrix, data.frame, list or environment containing numeric
    vectors of equal length.}
  
  \item{order.by}{time index. see below.}
  
  \item{states}{expression that evalutates to a numeric vector of
    consisting of 1, 0, and -1.}
  
  \item{size.at}{expression that evaluates to a logical vector
    stating those times when returns are compounded.}
  
  \item{roll.at}{expression that evaluates to a logical vector
    stating those times at which a roll trade is done.}
  
  \item{delta}{expression that evaluates to a numeric vector
    defining the percentage change in cumulative return given a change
    in price.}
  
  \item{prices}{expression containing names of data variables of object
    passed to \code{data} in a call to \code{tradesys.frame}.}
  
  \item{equity}{expression that evaluates to a numeric vector of the
    system's cumulative return.}

  \item{\dots}{ad-hoc quoted expressions.}
  
}

\details{

  A 'tradesys' object is a complete description of a trading
  system. \code{tradesys} returns its formal arguments as a named list
  with class attribute 'tradesys'. The argument values are first checked for
  consistency. A 'tradesys' object can be subsetted in the usual list way
  (the 'tradesys' class is stripped) and the \code{[.tradesys<-} and
  \code{$.tradesys<-} assignment methods call \code{tradesys} with the
  modified value. A \code{print} method prints the contents in a
  sensible manner and the \code{as.list} method and \code{as.tradesys}
  default method allow for translation to and from plain lists.

  \code{tradesys.frame} will evaluate a tradesys object passed to
  \code{x} with data passed to \code{data}.
  
  The expressions in stored in \code{x} are evaluated in a new
  environment that includes \code{data}, which is any numeric matrix,
  data.frame, list or environment containing all of the data variables
  contained in the expressions. 

  \code{order.by} is any vector whose objects can be computed by the
  \code{-} operator (its value is understood to be a measure of time
  interval) and is equal the number of rows in \code{data}.
  
  ??????????????
  
  Then all variables are combined into a data frame and returned.

}

\value{

  \code{tradesys} returns a tradesys object. \code{tradesys.frame}
  returns a data.frame of the evaluated expressions of the tradesys
  object passed to it. 

}

\examples{

library(TTR)

x <- tradesys()

## Assignment
x[["prices"]] <- quote(Close)
x$prices <- quote(Close) ## Same as above
x$states <- quote(signalmap(SMA(Close, 60) >= SMA(Close, 120), SMA(Close, 60) < SMA(Close, 120)))

## .. same as 'x' but all in one call
y <- tradesys(prices=Close, states=signalmap(SMA(Close, 60) >= SMA(Close, 120), SMA(Close, 60) < SMA(Close, 120)))
stopifnot(identical(x, y))

## .. same as x but via coercion of a list
stopifnot(identical(x, as.tradesys(as.list(x))))

## Extraction
x$prices
x$states
x[1:2]
x[["delta"]]

## Evaluation
x$AvgTR <- quote(na.locf(ATR(cbind(High,Low,Close), 14)[, 2], fromLast=TRUE))
x$delta <- quote(1/AvgTR) ## 1 ATR = 1pct chg in equity
y <- tradesys.frame(x, spx)
head(y)
tail(y)

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}