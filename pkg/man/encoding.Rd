\name{encoding}

\alias{addsig}
\alias{addcol}
\alias{entrywins}
\alias{entrywins<-}
\alias{entrycond}
\alias{entrycond<-}

\title{Trading System Encoding}

\description{
  
  Add to a \code{tsts} object expressions that define entry/exit conditions or
  derived column data.
  
}

\usage{
addsig(x, name, expr)
addcol(x, name, expr)

entrywins(x)
entrywins(x) <- value

entrycond(x)
entrycond(x) <- value

}

\arguments{
  \item{x}{a tsts object.}
  \item{expr}{expression.}
  \item{name}{character. signal name or column name.}
  \item{value}{logical. see below.}
}

\details{
  NEED TO DOCUMENT
}

\value{

  A \code{tsts} object with \code{expr} appended to its respective
  attribute. Evaluation via the \code{coredata} assignment method is a
  side-effect.
  
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
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}