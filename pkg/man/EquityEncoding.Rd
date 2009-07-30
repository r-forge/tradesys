\name{EquityEncoding}

\alias{delta}
\alias{delta<-}
\alias{size.at}
\alias{size.at<-}
\alias{roll.at}
\alias{roll.at<-}
\alias{percent}
\alias{percent<-}

\title{Trading System Equity Encoding}

\description{

  Define or evaluate a \code{tsts} object's equity-configuration expressions.
  
}

\usage{

delta(x)
delta(x) <- expression(value)

size.at(x)
size.at(x) <- expression(value)

roll.at(x)
roll.at(x) <- expression(value)

percent(x)
percent(x) <- value

}

\arguments{
  
  \item{x}{a "tsts" object.}
  \item{value}{an expression object.}
  
}

\details{
  \code{delta} is assigned an expression that must evaluate to a numeric
  vector (often a scalar) that specifies equity exposure as a function
  of price change. The expression is stored in the "delta" member of the "tsts"
  attribute.
  
  \code{roll.at} is assigned an expression that must evaluate to a logical
  vector specifying the times at which an open position is rolled. The
  expression is stored in the "roll.at" member of the "tsts"
  attribute. The performance calculations incorporate roll.at in the following
  way. For all those times where this vector is TRUE,
  an open long position is closed at the \emph{Short} price and
  simultaneously re-opened at the \emph{RollLong} price. Similarly, an
  open short position is closed at the \emph{Long} price and
  simultaneously re-opened at the \emph{RollShort} price.

  \code{size.at} is assigned an expression that must evaluate to a logical
  vector specifying the times at which a position is re-sized to
  delta. The expression is stored in the "size.at" member of the "tsts"
  attribute.

  Calling any of these three functions evaluates and returns its
  corresponding expresion with the named vectors of \code{x} and its
  index in the evaluation frame.
  
}

\examples{
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}