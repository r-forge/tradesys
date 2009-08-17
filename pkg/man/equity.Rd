\name{equity}

\alias{equity}
\alias{trades}

\title{Equity Curve and Trades}

\description{
  
  \code{equity} calculates an equity curve from \emph{prices},
  \emph{states}, and \emph{delta}.
  
  \code{trades} calculates a data.frame of trades from \emph{prices},
  \emph{states}, and \emph{delta}.
  
}

\usage{

equity(prices, states, delta=1, size.at=as.logical(c(states[1], diff(states))),
       roll.at=FALSE, percent=TRUE)

trades(prices, states, delta=1, roll.at=FALSE, percent=FALSE,
       order.by=index(prices))

}

\arguments{
  
  \item{prices}{numeric matrix or coerciable via \code{as.matrix}.}
  \item{states}{numeric vector of states.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  \item{delta}{numeric vector of delta coefficients.}
  \item{size.at}{logical. see below.}
  \item{roll.at}{logical. see below.}
  \item{percent}{logical. \emph{percentage} or \emph{absolute} change in price.}

}

\details{
  
  \code{equity} computes an equity value at each time \eqn{t} for a
  given pair of prices and states.
  
  \code{prices} should be coercable using \code{as.matrix}. The first
  column are prices and the second column are roll prices. (Use
  \code{"Price"} and \code{"Roll"} in the return of the \code{prices}
  function to construct these vectors out of data and states.) Any
  additional columns are ignored. If only a vector is passed, prices and
  roll prices are identical.
  
  \code{states} must be a vector consist of 1s, 0s, and -1s only.  Its
  length must be a multiple of \code{nrow(data)} and is generally
  identical to it.

  \code{delta} defines the percentage change in equity given a
  percentage change in price at each size.at time. It specifies the
  system's leverage parameter. If passed a scalar, the system follows a
  fixed fractional leveraging scheme. A variable leverage scheme is
  followed if a vector of varying values is passed. \code{delta} must
  have length that is a multiple of \code{nrow(data)}.
  
  \code{size.at} is a logical vector that causes the equity to have
  delta sensitivity at each time where this vector is TRUE. By default,
  TRUE values are inserted wherever a state change occurs, thus
  implementing the common scheme whereby trades are sized to delta
  equity at initiation and held constant throughout the trade horizon,
  resized only when a new trade entry occurs. \code{size.at=TRUE} will
  implement a dynamic leveraging scheme whereby a constant equity
  fraction is risked at each period. Any scheme can be defined by
  passing its appropriate vector, which must have length that is a
  multiple of length \code{nrow(data)}.

  Equity is initially normalised to 1, \eqn{E_{0} = 1}{E(0) = 1} and
  calculated thereafter as:

  \deqn{
    E_{t} = \left(\frac{p_{t}}{p_{i}} - 1\right) s_{t-1} \Delta E_{i} + E_{i} 
  }{E(t) = [p(t) / p(i) - 1] * s(t-1) * delta * E(i) + E(i)},
  where \eqn{i} is the time of the most recent \code{size.at} prior to
  \eqn{t}.

  As described above, the default meaning of \code{delta} is percentage
  change in equity for percentage change in price (at size.at
  times). If \code{percent} is false, \eqn{\Delta}{delta} is re-defined
  to mean: percentage change in equity given unit change in price (at
  size.at times). Equity is therefore calculated as:

  \deqn{
    E_{t} = (p_{t} - p_{i}) s_{t-1} \Delta E_{i} + E_{i}
  }{E(t) = [p(t) - p(i)] * s(t-1) * delta * E(i) + E(i)}.
  Using absolute rather than relative change is sensible in many
  contexts and necessary when price can take negative values (eg
  spreads).  

  If any \code{roll.at} are true, roll price adjustments are made. It is
  a logical vector, and if it is TRUE at \eqn{t}, \eqn{R_{t} -
  P_{t}}{R(t) - P(t)} is added to \eqn{P_{i}}{P(i)} in the calculation
  of \eqn{E_{t+1}}, where \eqn{R_{t}}{R(t)} is the roll price at
  \code{t}.

  \code{trades} is passed a tsts object and returns a data frame of
  the system's trades. Each row describes a long or short entry with its
  entry and exit times and prices, the realised P\&L of the trade and
  holding period return.
  
  \code{delta} is a numeric constant \eqn{\Delta}{d} that defines equity
  return per \emph{change in price} (or \emph{percentage change in price} if
  \code{percent=TRUE}). It defines the system's leverage.

  Trade holding period returns are defined as: \deqn{r = (p_{x} - p_{e})
  s_{e}\Delta}{r = [px - pe] * se * d} where \eqn{\Delta}{d} is the
  value passed to \code{delta} and \eqn{s_{e}}{se} is the state defining
  the trade's entry (1 or -1). If \code{percent} is TRUE, holding period
  returns are defined as:

  \deqn{r = (p_{x} / p_{e} - 1) s_{e}\Delta}
  {r = [px / pe -1] * se * d}. If \code{delta} is null, \eqn{\Delta}{d} is
  chosen so that the cumulative product of the holding period returns so
  defined is maximised.
  
  Roll trades are calculated when \code{roll.at} has at least one TRUE
  value, just as with the \code{equity} function. A roll at time
  \emph{t} is a pair of exit and entry trades--both at time
  \emph{t}--that close out an existing position with \eqn{P_{t}}{P(t)}
  and re-open it with \eqn{R_{t}}{R(t)}. So, a roll time that falls
  \emph{on or after} a trade's entry time and \emph{before} its exit
  time will be split into two such trades in the return. These are
  identified as such with an additional column named "roll".
  
}
  

\seealso{

  trades

}

\value{

  A matrix with columns "States", "Trade", "Size", "Roll", "Delta",
  "Prices", "PnL", "RoR", and "Equity". 
  
p}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}