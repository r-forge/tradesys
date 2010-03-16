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
  special environment constructed from \code{data} and returns the data,
  modified with the evaluated expressions.

  But whereas the base function is passed an expression, \code{exprlist}
  is a \emph{named list of quoted expressions}. Each expression in the
  list is evaluated, assigned to its corresponding name, and included in
  the return.

  Evaluation starts with the first member of \code{exprlist}, then the
  second, and so on. If an expression A is evaluated and A contains the
  name of another expression B, B is evaluated before A. Therefore, the
  members of \code{exprlist} may contain references to each other and
  \code{exprlist} may be ordered in any way. This is accomplished by
  evaluating the expressions via promises, and this evaluation framework
  is the main advantage of using \code{inside} over the base function
  \code{within}.

  The parent environment is that of the caller. 
  
}

\value{

  A list or a data.frame containing \code{data} and the evaluated
  members of \code{exprlist}.
  
}

\seealso{

  Class "expr.frame" and \code{within} (base package). 

}

\examples{

head(inside(spx, list(Ln=quote(log(RoR)), RoR=quote(ror(Close, 1, delta=.001)))))

}

\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}