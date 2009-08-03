\name{ConvenienceFunctions}

\alias{states}
\alias{states<-}
\alias{signals}
\alias{delta}
\alias{size.at}
\alias{roll.at}
\alias{prices}

\title{Convience Functions}

\description{

  Convience functions for manipulating a 'tsts' object.
  
}

\usage{

states(x)
states(x) <- value

prices(x, pricecols=tstsp(x)$pricecols, roll.at=FALSE)

signals(x) 
delta(x)
size.at(x)
roll.at(x)

}

\arguments{
  \item{x}{a tsts object.}
  \item{value}{a vector or expresion.}
  \item{pricecols}{list. see below.}
  \item{roll.at}{logical vector.}
}

\details{
  \code{states} simply returns the states vector. The assignment method
  expects a vector of states whose length is a multiple of
  \code{nrow(x)}. It triggers a recalculion of equity. Note that if the
  'signals' attribute is non-null, these values will be descarded the
  next time \code{x} is altered in any way.

  \code{signals} evaluates the signal expressions and returns their
  logical vectors in a four-column matrix. The assignment method defines
  an entry or exit rule, one-at-a-time. States are re-calculated upon
  each assignment. 
  
  \code{delta}, \code{size.at} and \code{roll.at} evaluate their
  respective expressions and return them in a vector.

  \code{prices} returns a matrix of \emph{Long}, \emph{Short},
  \emph{Mark}, \emph{RollLong}, and \emph{RollShort} prices in
  \code{x}. It also contains a column 'Price', which contains at each
  time one of long, short, or mark columns, depending upon the state,
  and the column 'Roll', which contains one of rolllong or rollshort
  prices, depending upon the state and \code{roll.at} value. 

}

\examples{

data(spx)
spx <- window(spx, start="2005-01-01")

x <- tsts(spx)

library(TTR)
tstsp(x)$el <- quote(SMA(Close, 60) >= SMA(Close, 120))
tstsp(x)$es <- quote(SMA(Close, 60) < SMA(Close, 120))

##tstsp(x)$exprcols <- quote(Foobar=Close * 2)

tail(signals(x))
##tail(exprcols(x))
delta(x)
tail(size.at(x))
tail(roll.at(x))
tail(prices(x))

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}