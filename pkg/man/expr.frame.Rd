\name{expr.frame}

\alias{expr.frame}
\alias{print.expr.frame}
\alias{summary.expr.frame}
\alias{exprlist}
\alias{$<-.expr.frame}
\alias{dimnames.expr.frame}
\alias{as.data.frame.expr.frame}
\alias{as.list.expr.frame}
\alias{as.matrix.expr.frame}
\alias{as.zoo.expr.frame}

\alias{coredata.expr.frame}
  
\title{Expression Frames}

\description{
  
  An object of class "expr.frame" is a data.frame with an attribute
  \code{exprlist} containing a named list of quoted expressions. The
  evaluation of these expressions are columns of the data frame. 
  
}

\usage{

expr.frame(x, exprlist)

exprlist(x)
  
}

\arguments{
  
  \item{x}{Object of class "expr.frame".}
  \item{exprlist}{a named list of quoted expressions.}

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
d <- expr.frame(d, list(RoR=quote(c(1, Close[-1] / Close[-length(Close)]))))
class(d); d

## We can now add/modify with the $ assignment method instead
d$Rtn <- quote(RoR - 1)
d$Close <- d$Close * 1.01
## d$RoR <- quote(ror(Close, delta=1/Close)) ## !!! Why Error? !!!
exprlist(d); d

## Retreive original data
coredata(d) 

## Coercion
as.data.frame(d)
as.list(d)
as.matrix(d)
as.zoo(d, order.by=rownames(d))

## Subsetting
d[1:2]
d$ror
## d[, 1:7] ## !!! FIX ME !!!

}

\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}