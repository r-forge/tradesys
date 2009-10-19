\name{RoR-PnL}

\alias{pnl}
\alias{ror}

\title{Returns and Profit/Loss}

\description{
  
  \code{ror} calculates a vector of returns.
  
  \code{pnl} calculates a vector of profit \& loss.

}

\usage{

ror(prices, states, delta=1, size.at=FALSE, roll.at=FALSE, period=FALSE)

pnl(prices, states, roll.at=FALSE)

}

\arguments{
  
  \item{prices}{numeric matrix or coerciable via \code{as.matrix}.}
  \item{states}{numeric ector of states.}
  \item{delta}{numeric. See below.}
  \item{size.at}{logical. See below.}
  \item{roll.at}{logical. See below.}
  \item{period}{logical. If true, return period instead of cumulative returns.}

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

  \code{roll.at} must be a logical vector or a numeric vector consisting
  of only 1 and 0. Its length must be a multiple of \code{nrow(data)}
  and is generally identical to it.
  
  \code{pnl} calculates the P&L in raw price points as follows:
  \deqn{
    PnL_{t} = \left(P_{t} - P_{t-1} + \left(I_{t-1} - O_{t-1}\right)R_{t-1}\right)s_{t-1}
  }{PnL(t) = [P(t) - P(t-1) + [I(t-1) - O(t-1)] * R(t-1)] * s(t-1)}

  \eqn{P} is the valuation price, \eqn{O} is the roll-out price,
  \eqn{I} is the roll-in price, and \eqn{R \in (0,1)}{R = (0,1)} is the
  roll flag.

  \code{ror} calculates the cumulative return as follows:

  \deqn{
    CRoR_{t} = \left(1 + \sum_{i=j+1}^{t} PnL_{i} \Delta_{j} \right)CRoR_{j}
  }{CRoR(t) = (1 + [(PnL(j+1) + PnL(j+2) + ... + PnL(t)) * delta(j)] * CRoR(j)}
  
  where \eqn{j} is the last TRUE \code{size.at} prior to \eqn{t}.

  \code{roll.at} allows the modelling of spliced price series (e.g.,
  'continuous' price series of futures contracts). Wherever
  \code{roll.at} is TRUE, it is assumed that the subsequent price
  observations in the price vector consist of the series hitherto
  contained in the roll-in vector. The above definition of PnL
  incorporates this by assuming that an open position is exited at the
  roll-out price and re-entered at the roll-in price at the time where
  \code{roll.at} is TRUE.

  \code{delta} defines the percentage change in cumulative return given
  change in price. \code{size.at} is a logical vector that specifies the
  times where returns are compounded. For example, if \eqn{t=9} and the
  last TRUE \code{size.at} prior to \eqn{t} is at 5 and the state is
  long since 5, then:
  
  \deqn{
    CRoR_{9} = (P_{9} - P_{6})\Delta_{6} CRoR_{5}
  }{CRoR(9) = [P(9) - P(6)] * delta(6) * CRoR(5)}
  
  Together \code{delta} and \code{size.at} define the system's leverage
  scheme. 
  
}
  

\value{
  
  A numeric vector representing P\&L in price points (\code{pnl}) or
  cumulative return (\code{ror}).
  
}

\examples{

Prices <- c(100,50,100) ## price down 50\%, up 50\%

ror(Prices, c( 1,-1, 1), delta=.01,  size.at=FALSE) ## c(1.0000, 0.5000, 0.0000)
ror(Prices, c( 1, 1, 1), delta=.01,  size.at=FALSE) ## c(1.0000, 0.5000, 1.0000))
ror(Prices, c( 1, 1, 1), delta=.01,  size.at=TRUE)  ## c(1.0000, 0.5000, 0.7500))
ror(Prices, c(-1, 1, 1), delta=.01,  size.at=FALSE) ## c(1.0000, 1.5000, 2.0000))
ror(Prices, c( 1, 1, 1), delta=.005, size.at=TRUE)  ## c(1.0000, 0.7500, 0.9375))
ror(Prices, c( 1, 1, 1), delta=.005, size.at=FALSE) ## c(1.0000, 0.7500, 1.0000))
ror(Prices, c( 1, 1, 1), delta=.015, size.at=TRUE)  ## c(1.0000, 0.2500, 0.4375))
ror(Prices, c( 1, 1, 1), delta=.015, size.at=FALSE) ## c(1.0000, 0.2500, 1.0000))

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}