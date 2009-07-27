\name{equity}

\alias{equity}

\title{Equity Curve}

\description{
  
  Calculate an equity curve given prices and states.

}

\usage{

equity(prices, states, delta=1, resize=NULL, percent=TRUE)

}

\arguments{
  
  \item{prices}{numeric vector of prices}
  \item{states}{numeric vector of states}
  \item{delta}{numeric vector of delta coeficients.}
  \item{resize}{logical. see below.}
  \item{percent}{logical. \emph{percentage} or \emph{absolute} change in price.}

}

\details{
  
  \code{equity} computes an equity value at each time
  \eqn{t} for a given pair of prices and states vectors.

  \code{prices} and \code{states} are generally the same length but must
  have lengths that are multiples of each other. \code{states} must
  consist of 1s, 0s, and -1s only.

  Equity is initially normalised to 1, \eqn{E_{0} = 1}{E(0) = 1} and
  calculated thereafter as:\deqn{
    E_{t} = [(\frac{p_{t}}{p_{i}} - 1) s_{t-1} \Delta E_{i}]
  }{E(t) = (p(t)/p(i) - 1) s(t-1) d E(i)}
  where \eqn{i} is the time of the last trade re-size prior to
  \eqn{t}. By default, trade resize occurs...  
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