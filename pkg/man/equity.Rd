\name{equity}

\alias{equity}

\title{Equity Curve and Period Returns}

\description{
  
  Calculate period returns and equity curve of a tsts object.

}

\usage{

equity(x, delta=NULL, percent=FALSE)

}

\arguments{
  
  \item{x}{a tsts object.}
  \item{delta}{numeric. Change in equity returns given change in price.}
  \item{percent}{logical. If TRUE, ...\emph{percentage} change in price.}

}

\details{
  
  \code{equity} computes a period return and equity value at each time
  \eqn{t} for a tsts object. It is therefore a higher resolution system
  performance analysis tool than that provided by \code{trades}.

  \code{delta} and \code{percent} carry the same meaning as they have in
  \code{trades}, although the per-time-unit resolution of the
  calculation means that \eqn{\Delta}{d} fluctuates inside the trade
  holding period; it gets smaller as PnL increases and larger as PnL
  decreases. 

  Period return is calculated as \deqn{ r_{t} = (p_{t} - p_{t-1})
    s_{t-1} \Delta_{t}}{r(t) = [p(t) - p(t-1)] * s(t-1) * d(t)}, unless
    \code{percent} is TRUE, in which case it is calculated as\deqn{
    r_{t} = (p_{t} / p_{t-1} - 1) s_{t-1} \Delta_{t}}{r(t) = [p(t) /
    p(t-1) - 1] * s(t-1) * d(t)}. \eqn{\Delta}{d} is defined as \deqn{
    \Delta_{t} = \prod_{i=j}^{t} \frac{\Delta_{j}}{1+r_{i}}}{d(t) =
    prod(d(j) / (1 + r(i))) for i's between j and t.} and
    \eqn{\Delta_{j}}{d(j)} is set to \code{delta}, where the last trade
    entry as of \eqn{t}{t} occurs at \eqn{j-1}{j-1}. In words,
    \eqn{\Delta_{t}}{d(t)} is \code{delta} times the reciprocal of
    cumlative period returns since the last trade entry.
  
  If \code{delta} is null, \eqn{\Delta}{d} is chosen so that the
  cumulative product of \emph{trade holding period returns} is
  maximised.
    
  Equity returns are defined as:\deqn{E_{t} = E_{t-1}(r_{t} + 1)}{E(t) =
  E(t-1)(r(t) + 1)} Initial equity, \eqn{E_{0}}{E(0)} is set to 1 and
  therefore \eqn{E_{t}}{E(t)} is identical to cumulative period returns
  at \eqn{t}{t}:
  
  Roll trades are calculated when when \code{x} has a non-null
  \code{tsts$roll.at} attribute. Rolls are used when a system runs on a
  spliced price timeseries (e.g., futures contracts). A roll at time
  \emph{t} is a pair of exit and entry trades that close out an existing
  position at the exit price and re-open it at the roll price, both at
  time \emph{t}. In \code{trades}, a roll time that falls within a
  trade's entry and exit times will be split into two. In \code{equity},
  a roll at \emph{t} is accounted for by setting \eqn{p_{t}}{p(t)} to
  the roll price in the calculation of \eqn{r_{t+1}}{r(t+1)}.

  The cumulative returns calculated by \code{trades} and \code{equity}
  will be exactly the same if identical values are passed to
  \code{delta}. (They will only differ in the edge case where enterlong
  and exitshort, or entershort and exitlong pricecols are different. In
  such a case, \code{trades} will allow different prices for a trade
  exit and entry done at the same time. \code{equity} on the other hand
  demands that one and only one price is used at each point in time; in
  this case, the entry pricecol is used and the exit pricecol is
  ignored.)
  
}

\seealso{

  trades

}

\note{
  
  The underlying assumption of these functions is "fixed equity
  fraction" leveraging. It would be nice to incorporate time varying
  point value to support leveraging schemes like "pyramiding",
  "volatility scaling", etc. if this can be done in a straightforward way.
  
}

\value{
  
  \code{equity} returns a matrix with columns "Trade", "St", "Delta", 
  "Price", "RoR", and "Equity". It is a zoo object with the same index as
  \code{x}.
  
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}