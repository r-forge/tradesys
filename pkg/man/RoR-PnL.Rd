\name{RoR-PnL}

\alias{pnl}
\alias{ror}

\title{Returns and Profit/Loss}

\description{
  
  \code{ror} calculates a vector of cumulative returns.
  
  \code{pnl} calculates a vector of period profit \& loss.

}

\usage{

ror(prices, states, roll.at=FALSE, delta=1, size.at=TRUE)

pnl(prices, states, roll.at=FALSE)

}

\arguments{
  
  \item{prices}{numeric matrix or coerciable via \code{as.matrix}.}
  \item{states}{numeric ector of states.}
  \item{roll.at}{logical. See below.}
  \item{delta}{numeric. See below.}
  \item{size.at}{logical. See below.}

}

\details{
  
  \code{prices} should be coercable using \code{as.matrix}. Valuation
  prices are in the first column, roll-out prices are in the second
  column, and roll-in prices are in the third column. If a two-column
  matrix is passed, the first column represents both valuation and
  roll-out prices, and roll-in prices are represented in the second. If
  only a vector is passed, no roll adjustments are made. Any columns
  beyond column three are ignored.
  
  \code{states} must be a vector consisting of 1, 0, and -1 only. Its
  length must be a multiple of \code{nrow(data)} and is generally
  identical to it.
  
  \code{pnl} calculates the P&L as:
  
  \deqn{
    PnL_{t} = \left(P_{t} - P_{t-1} + \left(I_{t-1} - O_{t-1}\right)R_{t-1}\right)s_{t-1}
  }{PnL(t) = [P(t) - P(t-1) + [I(t-1) - O(t-1)] * R(t-1)] * s(t-1)}

  \eqn{P} is the valuation price, \eqn{O} is the roll-out price,
  \eqn{I} is the roll-in price, and \eqn{R \in (0,1)}{R = (0,1)} is the
  roll flag passed to \code{roll.at}.
  
  \code{roll.at} must be a logical vector or a numeric vector consisting
  of only 1 and 0. Its length must be a multiple of \code{nrow(data)}
  and is generally identical to it. It allows the modelling of spliced
  price series (e.g., 'continuous' price series of futures
  contracts). Wherever \code{roll.at} is TRUE, it is assumed that
  subsequent observations in the valuation price vector consist of the
  series hitherto contained in the roll-in vector. The above definition
  of PnL incorporates this by assuming that an open position is exited
  at the roll-out price and re-entered at the roll-in price each time
  \code{roll.at} is TRUE.

  \code{ror} calculates the cumulative return as follows:

  \deqn{
    CRoR_{t} = \left(1 + \sum_{i=j+1}^{t} PnL_{i} \Delta_{j} \right)CRoR_{j}
  }{CRoR(t) = [1 + (PnL(j+1) + PnL(j+2) + ... + PnL(t)) * delta(j)] * CRoR(j)}
  
  where \eqn{j} is the last TRUE \code{size.at} prior to \eqn{t}.

  \code{delta} defines the percentage change in cumulative return given
  change in price. \code{size.at} is a logical vector that specifies the
  times where returns are compounded. By default \code{size.at=TRUE}, so
  returns are compounded every period and cumulative return is this
  intuitive special case of the above equation:

  \deqn{
    CRoR_{t} = \left(1 + PnL_{t}\Delta_{t-1}\right)CRoR_{t-1}
  }{CRoR(t) = [1 + PnL(t) * delta(t-1)] * CRoR(t-1)}
  
  This defines a leveraging scheme whereby open positions are re-sized
  at every period to maintain a constant \eqn{\Delta} sensitivity of
  price-to-equity, which may or may not be what is intended. A useful
  specification is \code{size.at=phasemap(states) != 0}, which defines
  the assumption that positions are sized to \eqn{\Delta} at initiation
  only. Another useful specification is \code{delta=n/prices}, which
  means that an \eqn{x\%} change in price (since the last TRUE \code{size.at})
  causes an \eqn{xn\%}{(x * n)\%} change in cumulative return. 
  
  Together \code{delta} and \code{size.at} define the system's leverage
  scheme. 
  
}
  

\value{
  
  A numeric vector representing P\&L in price points (\code{pnl}) or
  cumulative return (\code{ror}).
  
}

\examples{

  ## Intuitive examples: Price -.5, +.5
  ror(c(1.0,0.5,1.0), c( 1, 1, 1), delta=1.0,  size.at=FALSE) ## c(1.0000, 0.5000, 1.0000)
  ror(c(1.0,0.5,1.0), c( 1, 1, 1), delta=1.0,  size.at=TRUE)  ## c(1.0000, 0.5000, 0.7500)
  ror(c(1.0,0.5,1.0), c( 1, 1, 1), delta=0.5,  size.at=TRUE)  ## c(1.0000, 0.7500, 0.9375)
  ror(c(1.0,0.5,1.0), c( 1, 1, 1), delta=2.0,  size.at=TRUE)  ## c(1.0000, 0.0000, 0.0000)
  ror(c(1.0,0.5,1.0), c( 1, 1, 1), delta=1.5,  size.at=TRUE)  ## c(1.0000, 0.2500, 0.4375)
  ror(c(1.0,0.5,1.0), c( 1,-1, 1), delta=1.0,  size.at=FALSE) ## c(1.0000, 0.5000, 0.0000)
  ror(c(1.0,0.5,1.0), c( 1,-1, 1), delta=1.0,  size.at=TRUE)  ## c(1.0000, 0.5000, 0.2500)
  ror(c(1.0,0.5,1.0), c(-1, 1, 1), delta=1.0,  size.at=FALSE) ## c(1.0000, 1.5000, 2.0000)
  ror(c(1.0,0.5,1.0), c(-1, 1, 1), delta=0.5,  size.at=TRUE)  ## c(1.0000, 1.2500, 1.5625)

  ## Variable deltas
  ror(c(1.0,0.5,1.0), c(1,-1,1), delta=c(1,1,1), size.at=TRUE) ## c(1.00,0.50,0.25)
  ror(c(1.0,0.5,1.0), c(1,-1,1), delta=c(1,2,1), size.at=TRUE) ## c(1.00,0.50,0.00)
  ror(c(1.0,0.5,1.0), c(1,-1,1), delta=c(1,.5,1), size.at=TRUE)## c(1.00,0.50,.375)

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}