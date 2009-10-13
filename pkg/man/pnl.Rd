\name{PnL}

\alias{pnl}

\title{Profit \& Loss}

\description{
  
  \code{pnl} calculates a vector of profit \& loss from a vector of
  prices and states.
}

\usage{

pnl(prices, states, roll.at=FALSE)

}

\arguments{
  
  \item{prices}{numeric matrix or coerciable via \code{as.matrix}.}
  \item{states}{numeric vector of states.}
  \item{roll.at}{logical. see below.}
  
}

\details{
  
  \code{prices} should be coercable using \code{as.matrix}. Valuation
  prices are in the first column, roll-out prices are in the second
  column, and roll-in prices are in the third column. If a two-column
  matrix is passed, the first column represents both valuation and
  roll-out prices, and roll-in prices are represented in the second. If
  only a vector is passed, no roll adjustments are made. Any columns
  beyond column three are ignored.
  
  \code{states} must be a vector consisting of 1s, 0s, and -1s only. Its
  length must be a multiple of \code{nrow(data)} and is generally
  identical to it.

  \code{roll.at} must be a logical vector or a numeric vector consisting
  of only 1 and 0. Its length must be a multiple of \code{nrow(data)}
  and is generally identical to it.

  \deqn{
    PNL_{t} = \left(P_{t} - \left[P_{t-1} + \left(I_{t-1} - O_{t-1}\right)R_{t-1}\right]\right)s_{t-1}
  }{PNL(t) = [P(t) - [P(t-1) + [I(t-1) - O(t-1)] * R(t-1)]] * s(t-1)}

  \eqn{P} is the valuation price, \eqn{O} is the roll-out price,
  \eqn{I} is the roll-in price, and \eqn{R \in (0,1)}{R = (0,1)} is the
  roll flag. 
  
}
  

\value{

  A numeric vector representing simple profit \& loss in price points.
  
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}