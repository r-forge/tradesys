\name{TradesAndEquity}

\alias{equity}

\title{Trading System Performance Analysis}

\description{
  
  \code{equity} returns a tsts object's equity curve.\cr

}

\usage{

equity(x, delta=NULL, uselog=FALSE)

}

\arguments{
  
  \item{x}{a tsts object.}
  \item{delta}{numeric. Equity returns per change in price.}
  \item{uselog}{logical. If TRUE, re-define \code{delta} in terms of log
    returns.}
  
}

\details{
  
  \code{trades} is passed a tsts object and returns a data frame of
  the system's trades. Each row describes a long or short entry with its
  entry and exit times and prices, the realised P\&L of the trade and
  holding period return.
  
  \code{delta} is a numeric constant \eqn{\Delta}{d} that defines equity
  return per \emph{change in price}. It defines the system's
  leverage. Trade holding period returns are defined as: \deqn{r =
  (p_{x} - p_{e}) s_{e}\Delta}{r = [px - pe] * se * d} By default,
  \eqn{\Delta}{d} is chosen to maximise the geometric return over all
  trades.
  
  If \code{uselog=TRUE}, \eqn{p}{p} in the above equation is replaced
  with its natural logarithm, re-defining \eqn{\Delta}{d} as equity
  return per \emph{log price return}. 

  \code{equity} computes a period return and equity value at each time
  \eqn{t} for a tsts object. It is therefore a higher resolution system
  performance analysis tool than that provided by \code{trades}. Equity
  returns are defined as:\deqn{E_{t} = E_{t-1}(r_{t} + 1)}{E(t) =
  E(t-1)(r(t) + 1)} Initial equity, \eqn{E_{0}}{E(0)} is set to 1 and
  therefore \eqn{E_{t}}{E(t)} is identical to cumulative period returns
  at \eqn{t}{t}:
  
  \code{uselog} and \code{delta} carry the same meaning as they have in
  \code{trades}, although the per-time-unit resolution of the
  calculation means that \eqn{\Delta}{d} fluctuates inside the trade
  holding period; it gets smaller as PnL increases and larger as PnL
  decreases. Period return is calculated as: \deqn{r_{t} = (p_{t} -
  p_{t-1}) s_{t-1} \Delta_{t}}{r(t) = (p(t) - p(t-1)) * s(t-1) * d(t)}
  \eqn{\Delta}{d} is defined as \deqn{ \Delta_{t} = \prod_{i=j}^{t}
  \frac{\Delta_{j}}{1+r_{i}}}{d(t) = prod(d(j) / (1 + r(i))) for i's
  between j and t.}  and \eqn{\Delta_{j}}{d(j)} is set to \code{delta},
  where the last trade entry as of \eqn{t}{t} occurs at
  \eqn{j-1}{j-1}. In words, \eqn{\Delta_{t}}{d(t)} is \code{delta} times
  the reciprocal of cumlative period returns since the last trade entry.
  
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
  \code{delta}. When \code{delta=NULL} and the code optimises
  \eqn{\Delta}{d} to maximise cumulative return, the results will differ
  to some extent, as the optimisation is done on trade holding period
  returns in the trade case and period returns in the equity case. (They
  will also differ in the edge case where enterlong and exitshort, or
  entershort and exitlong pricecols are different. In such a case,
  \code{trades} will allow different prices for a trade exit and entry done at
  the same time. \code{equity} on the other hand demands that one
  and only one price is used at each point in time; in this case, the
  entry pricecol is used and the exit pricecol is ignored.)
  
}

\note{
  
  The underlying assumption of these functions is "fixed equity
  fraction" leveraging. It would be nice to incorporate time varying
  point value to support leveraging schemes like "pyramiding",
  "volatility scaling", etc. if this can be done in a straightforward way.
  
}

\value{
  
  \code{equity} returns a matrix with columns "trade", "states",
  "price", "ror", and "equity". It is a zoo object with the same index
  as \code{x}.
  
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}