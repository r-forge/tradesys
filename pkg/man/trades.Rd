\name{trades}

\alias{trades}

\title{Enumerate a Trading System's Trades}

\description{
  
  Return a tsts object's trades in a data frame. 
  
}

\usage{

trades(x, delta=NULL, useror=FALSE)

}

\arguments{
  
  \item{x}{a tsts object.}
  \item{delta}{numeric. Equity returns per change in price.}
  \item{useror}{logical. see below.}
  
}

\details{
  
  \code{trades} is passed a tsts object and returns a data frame of
  the system's trades. Each row describes a long or short entry with its
  entry and exit times and prices, the realised P\&L of the trade and
  holding period return.
  
  \code{delta} is a numeric constant \eqn{\Delta}{d} that defines equity
  return per \emph{change in price} (or \emph{price return} if
  \code{useror=TRUE}). It defines the system's leverage.

  Trade holding period returns are defined as: \deqn{r = (p_{x} - p_{e})
  s_{e}\Delta}{r = [px - pe] * se * d} where \eqn{\Delta}{d} is the
  value passed to \code{delta} and \eqn{s_{e}}{se} is the state defining
  the trade's entry (1 or -1). If \code{useror} is TRUE, holding period
  returns are defined as: \deqn{r = (p_{x} / p_{e} - 1) s_{e}\Delta}{r =
  [px / pe -1] * se * d}. If \code{delta} is null, \eqn{\Delta}{d} is
  chosen so that the cumulative product of the returns so defined is
  maximised.
  
  Roll trades are calculated when \code{x} has a non-null
  \code{tsts$roll.at} attribute. Rolls are used when a system runs on a
  continuous, or spliced timeseries (e.g., futures contracts). A roll at
  time \emph{t} is a pair of exit and entry trades that close out an
  existing position at the exit price and re-open it at the roll price,
  both at time \emph{t}. So, a roll time that falls within a trade's
  entry and exit times will be split into two such trades in the
  return. These are identified as such with an additional column named
  "roll".  }

\value{
  
  \code{trades} returns a data.frame with columns "Phase", "ETime",
  "XTime", "Time", "Numb", "EPrice", "XPrice", "PnL", "RoR", and (if
  present) "roll". Each row describes a trade.
  
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}