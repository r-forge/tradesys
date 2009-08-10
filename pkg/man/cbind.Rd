\name{cbind}

\alias{cbind.tsts}

\title{Add columns to a 'tsts' object.}

\description{
  
  \code{cbind} adds numeric vectors 'tsts' object's coredata.
  
}

\usage{

\method{cbind}{tsts}(\dots, deparse.level = 1)

}

\arguments{
  \item{\dots}{first argument is a 'tsts' object and the rest are
    numeric vectors.}
  \item{deparse.level}{numeric. currently ignored.}
}

\details{

  The numeric vectors being cbind'd must have a length that is a
  multiple of the number of rows in the 'tsts' object.
  
}

\value{
  
  A 'tsts' object with additional core data columns.
  
}

\examples{
##x <- matrix(rnorm(20), ncol=2, dimnames=list(NULL, c("Foo","Bar")))
##x <- tsts(x)
##cbind(x, Bam=1:10) 
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}