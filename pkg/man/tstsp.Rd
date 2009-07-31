\name{tsts}

\alias{tstsp}
\alias{tstsp<-}

\title{'tstsp' Attribute of Trading System Time Series Objects}

\description{

  \code{tstsp} returns the 'tstsp' attribute (or 'NULL'). The assignment
  method replaces this attribute or modifies a member of it. 

}

\usage{

tstsp(x)
tstsp(x) <- value

## Basic configuration
tstsp(x)$pricecols <- value
tstsp(x)$entrywins <- value
tstsp(x)$entrycond <- value
tstsp(x)$percent <- value

## Signal encoding
tstsp(x)$el <- expression(value)
tstsp(x)$es <- expression(value)
tstsp(x)$xl <- expression(value)
tstsp(x)$xs <- expression(value)

## Equity encoding
tstsp(x)$delta <- expression(value)
tstsp(x)$size.at <- expression(value)
tstsp(x)$roll.at <- expression(value)

}

\arguments{

  \item{x}{a 'tsts' object.}

}

\details{
  NEED TO DOCUMENT
}

\value{

  Named list

}

\examples{
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}