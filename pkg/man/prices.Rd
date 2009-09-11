\name{prices}

\alias{prices}

\title{Price Context}

\description{
  
  \code{prices} extracts a common price matrix from some data matrix.
  
}

\usage{

prices(x, states, pricemap, roll.at=FALSE)

}

\arguments{
  
  \item{x}{numeric matrix or object coerciable via \code{as.matrix}.}
  \item{states}{numeric vector of states.}
  \item{pricemap}{named character vector defining the mapping between
    price types and data variable names.}
  \item{roll.at}{logical. must be multiple of \code{nrow(x)}.}

}

\details{
  
  \code{pricemap} defines a one-to-one mapping between some \code{x}
  variables and five types of price information:
  
  \itemize{
    \item Mark
    \item Long
    \item Short
    \item RollOut
    \item RollIn
  }  
  
  These must be the names of the vector passed to \code{pricemap}. The
  corresponding values are the column names in \code{x} to which the
  price types are mapped. At least 'Mark' must be defined, but a scalar
  may be passed, in which case all five are given that value. Any of the
  five not specified are given the value assigned to 'Mark'. An error is
  raised if any \code{pricemap} is not in \code{colnames(x)}.
  
  The function returns a matrix of six columns: \code{Price},
  \code{Mark}, \code{Long}, \code{Short}, \code{RollIn}, and
  \code{RollOut} prices.

  The \code{Price} column is choosen in such a way that times where
  there is a long entry or short exit are assigned the corresponding
  price in \code{Long} column. Times where there is a short entry or
  long exit are assigned the corresponding price in the \code{Short}
  column, and unchanged times are assigned the price in the \code{Mark}
  column.
  
}
  
\examples{

x <- matrix(rnorm(150), ncol=3, dimnames=list(NULL, c("Bid","Ask","Close")))
(prices(x, c(1,1,0,-1,1), c(Mark="Close",Long="Ask",Short="Bid")))

}


\value{

  A matrix with columns "Price", "Mark", "Long", "Short", "RollOut" and
  "RollIn".
  
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}