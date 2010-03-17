\name{prices}

\alias{prices}

\title{Combining Multiple Price Vectors}

\description{
  
  \code{prices} returns a single price vector by selecting among
  alternative vectors on the basis of the phases of \code{states}.
  
}

\usage{

prices(states, uc, chg=uc, ent=chg, xit=chg, lng=chg, sht=chg,
       el=if(identical(ent, chg)) lng else ent,
       es=if(identical(ent, chg)) sht else ent,
       xl=if(identical(xit, chg)) sht else xit,
       xs=if(identical(xit, chg)) lng else xit)

}

\arguments{
  
  \item{uc}{numeric vector. prices mapped to phase 0 (unchanged).}
  \item{el}{numeric vector. prices mapped to phase 8 (enter long).}
  \item{es}{numeric vector. prices mapped to phase 4 (enter short).}
  \item{xl}{numeric vector. prices mapped to phase 2 (exit long).}
  \item{xs}{numeric vector. prices mapped to phase 1 (exit short).}
  \item{chg,ent,xit,lng,sht}{short-hand arguments.}
  \item{states}{numeric vector consisting of 1, 0, and -1 values.}
  
}

\details{

  This is a convenience function for constructing a single price vector
  from multiple vectors. Generally, all arguements get vectors of
  identical length, but all arguments must be passed vectors whose
  length is a multiple of the length of \code{states}. The return can be
  passed to the \code{prices} argument of functions \code{pnl} and
  \code{ror}. 
  
}

\value{
  
  A numeric vector of prices. 
  
}

\seealso{

  \code{pnl} and \code{ror}

}

\examples{

s <- sample(c(1,0,-1), 100, TRUE)
spx <- spx[1:100,]

## Example 1: entries/exits at Open, unchanged at Close.
(ex1 <- prices(s, spx$Close, chg=spx$Open))

## Example 2: entries/exits at next day's Open, unchanged at Close.
(ex2 <- prices(s, spx$Close, chg=c(spx$Open[-1], NA)))

## Example 3: same as above, but with 10bps slippage at entries/exits.
(ex3 <- prices(s, spx$Close, lng=c(spx$Open[-1], NA) * 1.001, sht=c(spx$Open[-1], NA) * .999))

}

\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}