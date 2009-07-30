\name{signals}

\alias{signals}
\alias{signals<-}

\title{Trading System Signal Encoding}

\description{

  Define or evaluate a \code{tsts} object's signal expressions.
  
}

\usage{

signals(x) 
signals(x, type="el") <- expression(value)

}

\arguments{
  \item{x}{a tsts object.}
  \item{type}{character. "el", "es", "xl", or "xs".}
  \item{value}{expression that evaluates to a logical vector.}
}

\details{
  NEED TO DOCUMENT
}

\examples{

data(spx)
x <- tsts(spx)

library(TTR)
x <- cbind(x, MAF=expression(SMA(Close, 60)))
x <- cbind(x, MAS=expression(SMA(Close, 120)))

head(x)
tail(x)

signals(x, "el") <- expression(MAF >= MAS)
signals(x, "es") <- expression(MAF < MAS)

head(signals(x))
tail(signals(x))

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}