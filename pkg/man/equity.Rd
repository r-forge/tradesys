\name{equity}

\alias{equity}
\alias{trades}

\title{Equity Curve and Trades}

\description{
  
  \code{equity} calculates an equity curve from \emph{prices},
  \emph{states}, and \emph{delta}.
  
  \code{trades} calculates a data.frame of trades from \emph{prices},
  \emph{states}, and \emph{delta}.
n  
}

\usage{

equity(prices, states, delta=1, size.at=FALSE, roll.at=FALSE, percent=TRUE)

trades(prices, states, delta=1, roll.at=FALSE, percent=FALSE,
       order.by=index(prices))

}

\arguments{
  
  \item{prices}{numeric matrix or coerciable via \code{as.matrix}.}
  \item{states}{numeric vector of states.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  \item{delta}{numeric vector of delta coefficients.}
  \item{roll.at}{logical. see below.}
  \item{size.at}{logical. see below.}
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
  resized only when a new trade entry occurs. Additional sizing times
  are specified with this parameter. A limiting case is
  \code{size.at=TRUE}, which implements a dynamic leveraging scheme
  whereby a constant equity fraction is risked at each period. 

  Equity is initially normalised to 1, \eqn{E_{0} = 1}{E(0) = 1} and
  calculated thereafter as:

  \deqn{
    E_{t} = \left(\frac{p_{t}}{p_{i}} - 1\right) s_{t-1} \Delta E_{i} + E_{i} 
  }{E(t) = [p(t) / p(i) - 1] * s(t-1) * delta * E(i) + E(i)},
  where \eqn{i} is the time of the most recent trade entry or
  \code{size.at} prior to \eqn{t}.

  As described above, the default meaning of \code{delta} is percentage
  change in equity for percentage change in price (at size.at
  times). If \code{percent} is false, \eqn{\Delta}{delta} is re-defined
  to mean: percentage change in equity given unit change in price (at
  size.at times), and the first term in the above equation is replaced with
  \deqn{p_{t} - p_{i}}{p(t) - p(i)}.
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
  
  Trade holding period returns are defined are implicit in the above
  equation that defines equity. Just replace \eqn{i} with the trade
  entry time and \eqn{t} with the trade exit time:
  \deqn{HPR = \left( \frac{P_{x}}{P_{e}} - 1 \right) S_{e} \delta_{e} =
    \frac{E_{x}}{E_{e}}}{
    HPR = (p(x) / p(i) - 1) * s(e) * d(e) = E(x) / E(e)
  }
  
  Roll trades are calculated when \code{roll.at} has at least one TRUE
  value, just as with the \code{equity} function. A roll at time
  \emph{t} is a pair of exit and entry trades--both at time
  \emph{t}--that close out an existing position with \eqn{P_{t}}{P(t)}
  and re-open it with \eqn{R_{t}}{R(t)}. So, a roll time that falls
  \emph{on or after} a trade's entry time and \emph{before} its exit
  will add \eqn{R_{t} - P_{t}}{R(t) - P(t)} to \eqn{P_{i}}.
  
}
  

\seealso{

  trades

}

\value{

  Equity returns a matrix with columns "Trade", "Pt", "St", "HPR",
  "Equity".
  
  Trades returns a dataframe with columns "Trade", "LS", "ETime",
  "XTime", "Numb", "Time", "EPrice", "XPrice", "Delta", "PnL", and
  "RoR".
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}