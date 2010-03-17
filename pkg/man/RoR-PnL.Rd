\name{RoR-PnL}

\alias{pnl}
\alias{ror}

\title{Returns and Profit/Loss}

\description{
  
  \code{ror} calculates a vector of cumulative returns.
  
  \code{pnl} calculates a vector of period profit & loss.

}

\usage{

ror(prices, states, roll.at=FALSE, size.at=TRUE, delta=1)

pnl(prices, states, roll.at=FALSE)

}

\arguments{
  
  \item{prices}{numeric matrix or coerciable via \code{as.matrix}.}
  \item{states}{numeric ector of states.}
  \item{roll.at}{logical. See below.}
  \item{size.at}{logical. See below.}
  \item{delta}{numeric. See below.}

}

\details{
  
  \code{prices} should be coercable by \code{as.matrix}. \emph{Valuation
  prices} are in the first column, \emph{roll-out} prices are in the
  second column, and \emph{roll-in} prices are in the third column. If a
  two-column matrix is passed, the first column represents both
  valuation and roll-out prices, and roll-in prices are represented in
  the second. If only a vector is passed, no roll adjustments are
  made. Any columns beyond column three are ignored.
  
  \code{states} must be a vector consisting of 1, 0, and -1 only. Its
  length must be a multiple of \code{nrow(data)} but is generally
  identical to it.
  
  \code{pnl} calculates the PnL as:
  
  \deqn{
    PnL_{t} = \left(P_{t} - P_{t-1} + \left(I_{t-1} - O_{t-1}\right)R_{t-1}\right)s_{t-1}
  }{PnL(t) = [P(t) - P(t-1) + [I(t-1) - O(t-1)] * R(t-1)] * s(t-1)}

  \eqn{P} is the valuation price, \eqn{O} is the roll-out price,
  \eqn{I} is the roll-in price, and \eqn{R \in (0,1)}{R = (0,1)} is the
  roll flag passed to \code{roll.at}.
  
  \code{roll.at} is a logical or numeric vector consisting of only 1 and
  0. Its length must be a multiple of \code{nrow(data)} but is generally
  identical to it. It allows the modelling of spliced price series
  (e.g., 'continuous' price series of futures contracts). Wherever
  \code{roll.at} is TRUE, it is assumed that subsequent observations in
  the valuation price vector continues the series hitherto contained in
  the roll-in vector. The above definition of PnL incorporates this by
  assuming that an open position is exited at the roll-out price and
  re-entered at the roll-in price each time \code{roll.at} is TRUE.

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
  specification is \code{size.at=phasemap(states)!= 0}, which defines
  the assumption that positions are sized to \eqn{\Delta} at initiation
  only. Another useful specification is \code{delta=n/prices}, which
  means that an \eqn{x\%} change in price (since the last TRUE \code{size.at})
  causes an \eqn{xn\%}{(x * n)\%} change in cumulative return. 
  
  Together \code{delta} and \code{size.at} define the system's leverage
  scheme. 
  
}
  

\value{
  
  A numeric vector representing PnL in price points (\code{pnl}) or
  cumulative return (\code{ror}).
  
}

\examples{

  prc <- c(1.0,0.5,1.0)

  ## Equity down 50%, unchanged
  ror(prc, c(1,1,1), delta=1.0, size.at=FALSE) 

  ## Equity down 50%, down 25%
  ror(prc, c(1,1,1), delta=1.0, size.at=TRUE) 

  ## Equity down 25%, down 6.25%
  ror(prc, c(1,1,1), delta=0.5, size.at=TRUE)

  ## Equity down 100% (bankrupt)
  ror(prc, c(1,1,1), delta=2.0, size.at=TRUE)  

  ## NOTE: Cum RoR <100% allowed!
  ror(prc, c(1,1,1), delta=3.0, size.at=TRUE)  

  ## Equity up 25%, up 50% 
  ror(prc, c(-1,1,1), delta=0.5, size.at=FALSE)

  ## Equity up 25%, up 56.25%
  ror(prc, c(-1,1,1), delta=0.5, size.at=TRUE) 

  ## These are the same.
  ror(prc, c(1,1,1), delta=c(1,2,1), size.at=TRUE) 
  ror(prc, c(1,1,1), delta=1, size.at=FALSE)

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}