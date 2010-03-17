\name{OptimalLeverage}

\alias{Kelly}
\alias{GambleGrowth}
\alias{GambleGrowthBin}
\alias{OptimalF}
\title{Optimal Leverage}

\description{
  Tools for calculating optimal leverage.
}

\usage{
OptimalF(payoffs, prob=NULL, by=.01)
Kelly(odds, prob)
GambleGrowth(payoffs, f, prob=NULL)
GambleGrowthBin(odds, prob, f=Kelly(odds, prob))
}

\arguments{
  \item{payoffs}{vector of gamble payouts.}
  \item{odds}{odds expressed as a ratio of win/loss.}
  \item{prob}{probability of a win.}
  \item{by}{resolution of "optimal f" solution.}
  \item{f}{fixed fraction of capital as value between 0 and 1.}
}

\details{
  Constant equity fraction trade sizing starts from the premise that we
  are playing a gamble with a positive expectation--an "edge"--and that
  our goal is to maximise geometric mean returns on that gamble (which
  is the same thing as maximising terminal wealth).

  In the 1950's John Kelly proved that for a binary gamble geometric
  growth is maximised when the equity fraction \eqn{f} equals the quantity
  \eqn{k}, defined in words as "edge over odds", in algebra as
  \deqn{
    k = \frac{edge}{odds} = p - \frac{(1 - p)}{b} = \frac{pw - 1}{w - 1}
  }{k = (pw - 1) / (w - 1)}
  where \eqn{w = b + 1}, your winning payout (odds plus your initial
  stake). The growth function of such a gamble is
  \deqn{
    G(f) = (1 - f + fw)^{p} (1 - f)^{(1-p)} - 1
  }{G(f) = (1 - f + fw)^p (1 - f)^(1-p) - 1}
  Kelly proved that this function is maximised when \eqn{G(f=k)}.

  \code{Kelly} computes \eqn{k} and \code{GambleGrowthBin} computes
  \eqn{G} for a binary gamble.

  When payouts are not binary (as in trading) we need a more general
  solution. One method is to specify a probability distribution. The
  growth function is then: \deqn{ G(f) = \prod_{i=1}^{n}
  \left(1+f\frac{-x_{i}}{min(x_{1}, x_{2}, ..., x_{n})}\right)^{p_{i}}
  }{G(f) = prod([1 + f(-xi / min(x1, x2, ..., xn))]^pi)} where
  \eqn{x_{i}}{xi} is the i-th payoff and \eqn{p_{i}}{pi} is its
  probability, where \eqn{p_{1} + p_{2} + ... + p_{n} = 1}{p1 + p2 +
  ... + pn}. In other words, the term gives a return for each gamble
  \eqn{i} on the assumption that the largest loss is \eqn{f} percent of
  equity. Instead of a discrete probability distribution we could
  simulate \eqn{x_{i}} from some process (or take them from historical
  data). When this is desired set \code{prob=NULL} and \eqn{p_{i}} will
  equal \eqn{1/n}.

  The generalised analog to kelly has been dubbed "optimal f", which we
  solve by calculating \eqn{G(f)} for a sequence of \eqn{f}'s
  between 0 and 1 and taking the maximum. \code{by} specifies the
  resolution of this iteration.
}
\references{
  "A New Interpretation of the Information Rate", J.L. Kelly, Jr.,
  \emph{AT&T Bell Labs Journal}, July 1956

  \emph{The Mathematics of Money Management}, Ralph Vince, 1992, John
  Wiley & Sons, Inc.
}

\examples{
library(stats)

## Optimal F limits to Kelly in the binomial case
binomial.simulation <- function(x){
  payoffs <- rbinom(x, 1, .5) * 2
  payoffs[which(payoffs == 0)] <- -1
  OptimalF(payoffs)
}
summary(sapply(rep(1000, 100), binomial.simulation))
Kelly(2, .5)

## Reconciles with Vince p.33
payoffs <- c(9,18,7,1,10,-5,-3,-17,-7)
OptimalF(payoffs)
GambleGrowth(payoffs, c(.16,.24))^9
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}
