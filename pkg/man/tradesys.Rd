\name{tradesys}

\alias{tradesys}
\alias{as.tsts.tradesys}
\alias{coredata.tradesys}
\alias{coredata<-.tradesys}

\title{Trading System Encoding}

\description{
  Create a "tradesys" object, a subclass of "tsts".
}

\usage{
tradesys(data, el=FALSE, es=FALSE, xl=FALSE, xs=FALSE,
         entrycond=FALSE, entrywins=FALSE, makecols=NULL, \dots)
}

\arguments{
  \item{data}{numeric vector or matrix.}
  \item{el,es}{expressions. entry rules.}
  \item{xl,xs}{expressions. exit rules.}
  \item{entrycond}{logical. see below.}
  \item{entrywins}{logical. see below.}
  \item{makecols}{list of expressions.}
  \item{\dots}{arguments passed to \code{tsts}.}
}

\details{
  NEED TO DOCUMENT
}

\value{
  A 'tradesys' object, a subclass of 'tsts'.
}

\examples{
library(TTR)
data(spx)
data(gbpusd)
x <- tradesys(spx, el=SMA(Close, 60) >= SMA(Close, 120),
              es=SMA(Close, 60) < SMA(Close, 120), pricecols="Close")
tail(x)
tail(coredata(x))
## !! FIX THIS !!
##coredata(x) <- gbpusd
##tail(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}