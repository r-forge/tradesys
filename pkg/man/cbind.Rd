\name{cbind}

\alias{cbind.tsts}

\title{Add columns to a 'tsts' object.}

\description{
  \code{cbind} adds a numeric vector or expression to a 'tsts' object's columns.
}

\usage{
\method{cbind}{tsts}(\dots, deparse.level = 1)
}

\arguments{
  \item{\dots}{first argument is a 'tsts' object and the rest are
    vectors or (with \code{cbind}) expressions.}
  \item{deparse.level}{numeric. currently ignored.}
}

\details{
  NEED TO DOCUMENT
}

\value{
  A 'tsts' object with additional columns.
}

\examples{
x <- matrix(rnorm(20), ncol=2, dimnames=list(NULL, c("Foo","Bar")))
x <- tsts(x)

## data 
cbind(x, Bam=1:10) 

## call 
cbind(x, Bin=quote(Foo * pi)) 

## both.. note the column placement
cbind(x, Bin=quote(Foo * pi), Bam=1:10)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}