\name{encoding}

\alias{addsig}
\alias{addcol}
\alias{getsig}
\alias{getcol}
\alias{entrywins}
\alias{entrywins<-}
\alias{entrycond}
\alias{entrycond<-}

\title{Trading System Encoding}

\description{

  Define or retrieve a \code{tsts} object's signals or column expressions.
  
}

\usage{

addsig(x, name, expr)
addcol(x, name, expr)

getsig(x)
getcol(x)

entrywins(x)
entrywins(x) <- value

entrycond(x)
entrycond(x) <- value

}

\arguments{
  \item{x}{a tsts object.}
  \item{expr}{expression. signal or column definition.}
  \item{name}{character. signal or column name.}
  \item{value}{logical. see below.}
}

\details{
  NEED TO DOCUMENT
}

\examples{
data(spx)
x <- tsts(spx)

library(TTR)
addcol(x, "MAF", SMA(Close, 60))
addcol(x, "MAS", SMA(Close, 120))

head(x)
tail(x)

addsig(x, "el", MAF >= MAS)
addsig(x, "es", MAF < MAS)

head(x)
tail(x)

entrywins(x)
entrywins(x) <- TRUE
entrycond(x)
entrycond(x) <- TRUE

getcol(x)
getsig(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}