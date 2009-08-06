\name{states}

\alias{states}
\alias{states<-}

\title{States}

\description{

  Extract (assign) the states vector from (to) a trading system time series.
  
}

\usage{

states(x)
states(x) <- value

}

\arguments{
  
  \item{x}{a tsts object.}
  \item{value}{a vector or expresion.}
  
}

\details{
  
  \code{states} simply returns the states vector. The assignment method
  expects a vector of states whose length is a multiple of
  \code{nrow(x)}. It triggers a recalculion of equity. Note that if the
  'signals' attribute is non-null, these values will be descarded the
  next time \code{x} is altered in any way.

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}