\name{EquityStats}

\alias{EquityStats}

\title{Equity Statistics}

\description{
  Various equity performance statistics.
}

\usage{
EquityStats(equity, dates=index(equity))
}

\arguments{
  \item{equity}{numeric. vector of equity values.}
  \item{dates}{Date. date index for \code{equity}.}
}

\details{
  The following performance statistics are calculated for the
  date-indexed vector of equity values passed to the function.

  Two return statistics are calculated:
  \tabular{ll}{
    RORC =\tab \eqn{E_{n} / E_{1} - 1}\cr
    CAGR =\tab \eqn{(E_{n} / E_{1})^{(365/D)} - 1}
  }
  where \eqn{E_{i}} is equity at time \emph{i} and \emph{D} is number of
  days between \emph{Date(n)} and \emph{Date(1)}.
  
  Two volatility statistics are calculated:
  \tabular{ll}{
    MAXDD =\tab \eqn{MIN(DD_{1}, ..., DD_{n})}\cr
    VOLA =\tab \eqn{SD(ROR_{1}, ..., ROR_{n}) * \sqrt{365/D}}\cr
  }
  where \eqn{DD_{i}} is the max drawdown at time \emph{i}, defined as \deqn{
    min(E_{1}, ..., E_{i}) / E_{i} - 1
  }

  We compute the following regression: \deqn{
    \frac{E_{t}}{E_{0}} - 1 = t(1/365)r + \epsilon
  }
  ROR\% is the slope of this regression and R2 is its r-squared.
}

\value{
  A named vector of performance statistics RORC, CAGR, ROR\%, VOLA, MAXDD, and R2.
}

\examples{
## What everybody wants..
EquityStats(zoo(seq(1.01, length.out=12, by=.01),
            order.by=seq(Sys.Date(), length.out=12, by="month")))

## S&P 500
data(spx)
EquityStats(spx[, "Close"])
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}
