\name{tsr}

\alias{tsr}

\title{Trading System Returns}

\description{
  
  \code{tsr} calculates a vector of period returns of a trading
  system's equity.
  
}

\usage{

tsr(prices, states=1, delta=1/prices)

}

\arguments{
  
  \item{prices}{numeric vector of prices.}
  \item{states}{numeric vector of states.}
  \item{delta}{numeric. See below.}

}

\details{
  
  \code{prices} must be a numeric vector. 

  \code{states} must be a vector consisting of 1, 0, and -1 only. Its
  length must be a multiple of \code{prices} but is generally
  identical to it.
  
  \code{tsr} calculates the trading system return at each period
  \emph{t} as
  \deqn{
    (P_{t} - P_{t-1})S_{t-1}\Delta_{t-1}
  }{[P(t) - P(t-1)] * S(t-1) * D(t-1)}
  where \eqn{\Delta_{t}}{D(t)} is \code{delta[t]}.

  The default value of \code{delta} calculates the familiar special case
  of the above formula
  \deqn{(P_{t} / P_{t-1} - 1) S_{t-1}}{(P(t) / P(t-1) - 1) * S(t-1)}
  , arithmetic period returns.
  
}
  

\value{
  
  A numeric vector of period returns.
  
}

\examples{

p <- c(100,110,100,90,120)

## Arithmetic returns by default. x% chg in price = x% chg in equity.
c(0, exp(diff(log(p))) - 1)
tsr(p)
tsr(p, c(1,-1,-1,1,0))

## 2x leveraged
tsr(p, delta=2/p)

## 1 point chg in price = 1% change in equity.
tsr(p, delta=.01)

## The 'Equity Curve'
cumprod(tsr(p) + 1)

plot(sapply(seq(.5, to=5, by=.5), function(lev) prod(tsr(p) * lev + 1)), type="l",
     main="Don't forget that returns are non-linear with leverage!", sub="Leverage Ratio")

## 2x leveraging of price returns
tsr(p, delta=2/p)
tsr(p) * 2  ## same

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}