\name{inside}

\alias{inside}

\title{Evaluate a List of Expressions in a Data Environment}

\description{
  
     Do lazy evaluation on a named list of quoted R expressions in an
     environment constructed from data, modifying the original data.
  
}

\usage{

inside(data, exprlist, ...)

}

\arguments{
  
  \item{data}{A data.frame or list.}

  \item{exprlist}{A named list of quoted expressions.}

  \item{\dots}{Ignored.}

}

\details{

  This is a dialect of the base function \code{within}. Like the base
  function, \code{inside} evaluates the expression(s) passed to it in a
  special environment constructed from \code{data} and returns the data
  modified with the evaluated expressions.

  But whereas the base function is passed an expression, \code{exprlist}
  is a named list of quoted expressions. Each expression in the list is
  evaluated and assigned to its corresponding name in the
  return. The expressions may contain references to each other
  (non-circular, of course). The order of the quoted expressions in
  \code{exprlist} does not matter, as they are evaluated via
  promises. This is the main advantage of using \code{inside} over the
  base function \code{within}.

  The parent environment is that of the caller. 
  
}

\value{
  
}

\seealso{

  Class "expr.frame" and \code{within} (base package). 

}

\examples{

head(inside(spx, list(Ln=quote(log(RoR)), RoR=quote(ror(Close, 1, delta=.001)))))

stopifnot(identical(spx, inside(spx, NULL))) # does nothing if passed NULL

}

\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}