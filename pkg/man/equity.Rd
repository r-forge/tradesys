\name{equity}

\alias{equity}

\title{Equity Curve}

\description{
  
  Calculate an equity curve given prices and states.

}

\usage{

equity(prices, states, delta=1, size.at=NULL, roll.at=FALSE, percent=TRUE)

}

\arguments{
  
  \item{prices}{numeric vector of prices}
  \item{states}{numeric vector of states}
  \item{delta}{numeric vector of delta coefficients.}
  \item{size.at}{logical. see below.}
  \item{roll.at}{logical. see below.}
  \item{percent}{logical. \emph{percentage} or \emph{absolute} change in price.}

}

\details{
  
  \code{equity} computes an equity value at each time
  \eqn{t} for a given pair of prices and states vectors.

  \code{prices} and \code{states} are generally the same length but must
  have lengths that are multiples of each other. \code{states} must
  consist of 1s, 0s, and -1s only.

  \code{delta} defines the percentage change in equity given a
  percentage change in price at each trade size.at point. It specifies
  the system's leverage parameter. If passed a scalar, the system
  follows a fixed fractional leveraging scheme. A variable leverage
  scheme is followed if a vector is passed. In such a case, the values
  are usually the same length as \code{prices} and
  \code{states}. Differing lengths are allowed but must be a multiple of
  these.
  
  \code{size.at} is a logical vector that causes the equity to have delta
  sensitivity at each point where this vector is TRUE. By default, TRUE
  values are inserted wherever a trade entry occurs, thus implimenting
  the common scheme whereby trades are sized to delta equity at
  initiation and held constant throughout the trade
  horizon. \code{size.at=TRUE} will impliment a dynamic leveraging scheme
  whereby a constant equity fraction is risked at each period. Any
  scheme can be defined by passing its appropriate vector, which length
  must be a multiple of length \code{price}.

  Equity is initially normalised to 1, \eqn{E_{0} = 1}{E(0) = 1} and
  calculated thereafter as:\deqn{
    E_{t} = \left(\frac{p_{t}}{p_{i}} - 1\right) s_{t-1} \Delta E_{i} + E_{i}
  }{E(t) = [p(t) / p(i) - 1] * s(t-1) * delta * E(i) + E(i)}, 
  where \eqn{i} is the time of the last trade re-size prior to
  \eqn{t}, as specified in \code{size.at}.

  As described above, the default meaning of delta is percentage change
  in equity for percentage change in price (at trade size.at points). If
  \code{percent} is false, \eqn{\Delta}{delta} is re-defined to mean:
  percentage change in equity given unit change in price (at trade
  size.at points). Equity is therefore calculated as\deqn{
    E_{t} = (p_{t} - p_{i}) s_{t-1} \Delta E_{i} + E_{i}
  }{E(t) = [p(t) - p(i)] * s(t-1) * delta * E(i) + E(i)}
  . Using absolute rather than relative change is sensible in many
  contexts and necessary when price can take negative values (eg many spreads
  and derivative positions).

  If any \code{roll.at} are true, roll price adjustments are made. It is
  a logical vector, and everywhere it is true, \eqn{R_{t} - P_{t}}{R(t)
    - P(t)} is added to \eqn{P_{i}}{P(i)}, where \eqn{R_{t}}{R(t)} is
  the price in \code{prices[t, 2]}. That is, a two-column matrix should
  be passed to \code{prices}, with the second column containing the roll
  prices. If this is not done, \code{roll.at} is ignored.
  
}
  

\seealso{

  trades

}

\value{

  A matrix with columns "States", "Trade", "Delta", "Prices", "PnL", "RoR", and "Equity". 
  
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}