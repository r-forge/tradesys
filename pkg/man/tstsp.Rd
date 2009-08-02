\name{tsts}

\alias{tstsp}
\alias{tstsp<-}

\title{'tstsp' Attribute of Trading System Time Series Objects}

\description{

  \code{tstsp} returns the 'tstsp' attribute (or 'NULL'). The assignment
  method modifies this attribute and recalculates the system.

}

\usage{

tstsp(x)
tstsp(x) <- value

}

\arguments{

  \item{x}{a 'tsts' object.}

}

\details{
  Note that assigning directly to an expression member of 'tstsp'
  requires a call to \code{expression} on the right-hand side, eg,
  \code{tstsp(x)$size.at <- expression(SomeExpr)}. 
}

\value{

  Named list

}

\examples{
data(spx)
spx <- window(spx, start="2005-01-01")
tstsp(x <- tsts(spx))
library(TTR)
tstsp(x)$signals$el <- expression(SMA(Close, 60) > SMA(Close, 120))
tstsp(x)$signals$es <- expression(SMA(Close, 60) < SMA(Close, 120))
tstsp(x)$entrycond
tstsp(x)$entrycond <- TRUE
tail(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}