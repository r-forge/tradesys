\name{prices}

\alias{prices}

\title{Time Series Prices Context}

\description{

  \code{prices} extracts from \code{x} a price context matrix.
  
}

\usage{

prices(x, pricemap, states, roll.at=FALSE)

}

\arguments{
  \item{x}{a tsts object.}
  \item{pricemap}{named character vector. see below.}
  \item{states}{numeric vector. must be multiple of nrow(x).}
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