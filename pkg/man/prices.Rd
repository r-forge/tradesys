\name{prices}

\alias{prices}

\title{Time Series Prices}

\description{

  Extract prices from a trading system time series.
  
}

\usage{

prices(x, pricecols=tsys(x)$pricecols, roll.at=FALSE)

}

\arguments{
  \item{x}{a tsts object.}
  \item{pricecols}{list. see below.}
  \item{roll.at}{logical vector.}
}

\details{
  
  \code{prices} returns a matrix of \emph{Long}, \emph{Short},
  \emph{Mark}, \emph{RollLong}, and \emph{RollShort} prices in
  \code{x}. It also contains a column 'Price', which contains at each
  time one of long, short, or mark columns, depending upon the state,
  and the column 'Roll', which contains one of rolllong or rollshort
  prices, depending upon the state and \code{roll.at} value. 

}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}