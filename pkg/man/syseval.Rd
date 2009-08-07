\name{syseval}

\alias{syseval}

\title{Evaluate a Trading System with Data}

\description{

  \code{syseval} calculates state, equity, signal and other
  vectors of system \code{x} by evaluating its expressions with
  \code{data}.

}

\usage{

syseval(x, data, order.by=index(data))

}

\arguments{

  \item{x}{a 'tsys' object.}
  \item{data}{a numeric matrix.}
  \item{order.by}{an index vector with unique entries by which the observations
    in 'data' are ordered.}
  
}

\details{
  The states and equity vectors of a system applied to data are
  calculated by evaluating its expressions in a specific and
  straight-forward way. The expressions are evaluated in a frame that
  includes each column of \code{data} as a vector named by its column
  name. A vector is computed on by including its name in an
  expression.

  The evaluation frame of the expression is actually just the frame of
  the \code{syseval} function call with the data vectors added to it. In
  this document we'll refer to this simply as "the frame".

  \code{data} must be a numeric matrix and \code{x$datacols} must be a
  subset of \code{colnames(data)}. The length of \code{order.by} must
  equal the number of rows in \code{data}.

  The evaluation procedure is as follows. First, if \code{x$formulae} is
  non-null, its list of expressions are evaluated in order and the
  resulting vectors are placed in the frame so they can be referred to
  by the other expressions.

  Second, the entry and exit
  signal expressions \code{x$el}, \code{x$es}, \code{x$xl}, and
  \code{x$xs} are evaluated. The resulting four vectors are passed to
  the function \code{signalmap} to compute the states
  vector. \code{x$entrycond} and \code{x$entrywins} are logical options
  that modify this calculation. See the documentation of
  \code{signalmap} for details. The states vector is given the name 'St'
  and included in the frame.

  Third, delta, size.at, and roll.at are evaluated. Their results are
  included in the frame only after each is evaluated, so the expressions
  \emph{cannot} refer to each other.

  Finally, the equity vector is calculated using the values in the frame
  plus \code{x$percent} and \code{x$pricecols}. See its documentation
  for details.

  The results are collected into a named list and returned.

  \code{syseval} is called by \code{tsts}. As the trading system time
  series returned by that function includes only the 'St' and 'Equity'
  vectors in its result, a user may wish to use the construct
  \code{syseval(tsys(x), coredata(x))} to extract the other evaluations
  implicit in a 'tsts' object \code{x}.
  
}

\value{

  A named list consisting of the calculated vectors: states, equity,
  entry and exit signals, delta, size.at, roll.at and formulae matrix.

}

\examples{
x <- tradesys(c("Foo","Bar"), el=Foo > Bar, es=Foo <= Bar)
m <- matrix(rnorm(100), ncol=2, dimnames=list(NULL, c("Foo","Bar")))
syseval(x, m)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}