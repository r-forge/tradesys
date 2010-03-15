\name{expr.frame}

\alias{exprlist}
\alias{exprlist<-}
\alias{"$<-.expr.frame"}
\alias{as.data.frame.expr.frame}
\alias{as.list.expr.frame}
\alias{print.expr.frame}
\alias{coredata.expr.frame}
  
\title{Expression Frames}

\description{
  
  An object of class "expr.frame" is a data.frame with an attribute
  \code{exprlist} containing a named list of quoted expressions. The
  evaluation of these expressions are columns of the data frame. 
  
}

\usage{

exprlist(x)
exprlist(x) <- value
  
}

\arguments{
  
  \item{x}{a data.frame or expr.frame.}
  \item{value}{a named list of quoted expressions.}

}

% \details{
% }

% \value{  
% }

\seealso{

  \code{inside}

}

\examples{

d <- spx[1:10,]

## exprlist assigment creates 'expr.frame' object
exprlist(d)$RoR <- quote(c(1, Close[-1] / Close[-length(Close)]))
class(d); d

## We can now modify with the $ assignment method instead
d$Rtn <- quote(RoR - 1)
exprlist(d); d

## Retreive original data
coredata(d) 
## stopifnot(identical(coredata(d), d[1:5,]))

## Coercion
as.data.frame(d)
as.list(d)

## Subsetting
d[1:2]
d$ror
## d[, 1:7] ## !!! FIX ME !!!

## Assign NULL to remove an expression
exprlist(d)$Rtn <- NULL
d

## NULL assigned to the list itself will strip the expr.frame class
exprlist(d) <- NULL
class(d); d


}

\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}