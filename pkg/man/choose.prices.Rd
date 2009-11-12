\name{choose.prices}

\alias{choose.prices}

\title{Combining Multiple Price Vectors}

\description{
  
  \code{choose.prices} returns a single price vector by selecting among
  alternative vectors on the basis of the phases of \code{states}.
  
}

\usage{

choose.prices(states, uc, chg=uc, ent=chg, xit=chg, lng=chg, sht=chg,
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

  This is a convenience function for use in expressions passed to the
  \code{prices}, \code{rollin}, and \code{rollout} arguments of
  \code{tradesys}. Generally, all arguements get vectors of identical
  length, and all arguments must be passed vectors whose length is a
  multiple of the length of \code{states}.
  
}

\value{
  
  A vector of prices. 
  
}

\examples{

s <- sample(c(1,0,-1), 100, TRUE)
spx <- coredata(spx[1:100,])

## Example 1: entries/exits at Open, unchanged at Close.
(ex1 <- choose.prices(s, spx[, "Close"], chg=spx[, "Open"]))

## Example 2: entries/exits at next day's Open, unchanged at Close.
(ex2 <- choose.prices(s, spx[, "Close"], chg=c(spx[-1, "Open"], NA)))

## Example 3: same as above, but with 10bps slippage at entries/exits.
(ex3 <- choose.prices(s, spx[, "Close"], lng=c(spx[-1, "Open"], NA) * 1.001, sht=c(spx[-1, "Open"], NA) * .999))

## Unit Tests
p <- choose.prices(s, rep(0, 100), el=rep(8, 100), es=rep(4, 100), xl=rep(2, 100), xs=rep(1, 100))
stopifnot(identical(phasemap(s), p)) 

################################################################
## Why the rounding error at the 15th dec. place?
## stopifnot(identical(ex3 - ex2, ex2 * .001 * changemap(s)))
################################################################

stopifnot(identical(round(ex3 - ex2, 14), round(ex2 * .001 * changemap(s), 14)))

}

\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}