\name{encoding}

\alias{addsig}
\alias{addcol}

\title{Trading System Encoding}

\description{
  
  Add to a \code{tsts} object expressions that define entry/exit conditions or
  derived column data.
  
}

\usage{
addsig(x, name, expr)
addcol(x, name, expr)
}

\arguments{
  \item{x}{a tsts object.}
  \item{expr}{expression.}
  \item{name}{character. signal name or column name.}
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
spxx <- tsts(spx)

addcol(spxx, "Diff", Open - Close)
addcol(spxx, "Biff", Open * 2)
addsig(spxx, "el", Open < Close)
addsig(spxx, "es", Open > Close)

tail(spxx)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}