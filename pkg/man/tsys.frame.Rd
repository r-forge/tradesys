\name{tsys.frame}

\alias{tsys.frame}

\title{Evaluate a Trading System with Data}

\description{

  \code{tsys.frame} returns a data.frame containing the evaluations of
  \code{x} using the variables in \code{data}. In addition, the states
  and equity vectors are calculated.

}

\usage{

tsys.frame(x, data, order.by=index(data))

}

\arguments{

  \item{x}{a 'tsys' object.}
  \item{data}{a numeric matrix, data.frame, list or environment
    containing the variables in \code{datavars}.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  
}

\details{
  
  The states and equity values of a system when it is applied to data
  are calculated by evaluating its expressions in a certain way. The
  expressions are evaluated in a new environment that includes
  \code{data}, which is any numeric matrix, data.frame, list or
  environment containing all of the variables in \code{datavars} and
  where all of the variables have the same length.

  \code{order.by} is any vector whose objects can be computed by the
  \code{-} operator (its value is understood to be a measure of time
  interval) and is equal the number of rows in \code{data}.

  The evaluation procedure is as follows.

  \enumerate{
    \item Place \bold{data} and \bold{index} in a new environment \emph{env}.
    \item Evaluate and assign \bold{el}, \bold{es}, \bold{xl} and \bold{xs} in \emph{env}.
    \item Call \code{signalmap} and assign \bold{states} to \emph{env}.
    \item Evaluate and assign \bold{exprvars} to \emph{env}.
    \item Evaluate and assign \bold{roll.at} in \emph{env}.
    \item Call \code{prices} and assign its variables to \emph{env}.
    \item Evaluate and assign \bold{size.at} and \bold{delta} in \emph{env}.
    \item Call \code{equity} and assign \bold{equity} to \emph{env}.
  }
  
  Each variable is checked to ensure it has the correct length and
  type. Then all variables are combined into a data frame and returned.
  
}

\value{

  data.frame consisting of the calculated vectors: states, equity,
  entry and exit signals, delta, size.at, roll.at and exprvars.

}

\examples{
library(TTR)
x <- tradesys(c("Open","Close"))
x$el <- quote(SMA(Close, 60) > SMA(Close, 120))
x$es <- quote(SMA(Close, 60) <= SMA(Close, 120))
y <- tsys.frame(x, spx)
tail(y)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}