\name{prices}

\alias{prices}

\title{Price Context}

\description{
  
  \code{prices} returns a common price matrix from some data matrix and
  states vector.
  
}

\usage{

prices(x, states, pricemap)

}

\arguments{
  
  \item{x}{numeric matrix, object coerciable via \code{as.matrix} or of
    class 'tsts'.}
  \item{states}{numeric vector of states.}
  \item{pricemap}{named character vector defining the mapping between
    price types and data variable names.}

}

\details{
  
  When \code{x} is class 'tsts', any values passed to the other two are
  ignored and replaced with \code{x[, "St"]} and
  \code{tsys(x)$pricemap}.

  \code{pricemap} specifies which variables in \code{x} contain which
  price information. There are five categories of price information that
  need to be mapped to data variables:
  
  \itemize{
    \item Mark
    \item Long
    \item Short
    \item RollLong
    \item RollShort
  }  
  
  These must be the names of the vector passed to \code{pricemap}. The
  corresponding values are the column names in \code{x} to which they
  are mapped. The vector need not specify all five. By default, 'Long'
  and 'Short' are keyed to 'Mark', 'RollLong' to 'Long' and 'RollShort'
  to 'Short'. At least 'Mark' must be defined, but a scalar may be
  passed, in which case all five are given that value. An error is
  raised if \code{colnames(x)} does not contain a column identifier that
  you pass.
  
  The function returns a matrix of seven columns: \code{Price},
  \code{Roll}, \code{Mark}, \code{Long}, \code{Short}, \code{RollLong},
  and \code{RollShort} prices. Columns 3-7 simply extract the relevant
  columns from \code{x} on the basis of the \code{pricemap}.

  The \code{Price} column is choosen in such a way that times where
  there is a long entry or short exit are assigned the corresponding
  price in \code{Long} column. Times where there is a short entry or
  long exit are assigned the corresponding price in the \code{Short}
  column, and unchanged times are valued using the price in the
  \code{Mark} column.

  The \code{Roll} column contains the value in \code{RollLong},
  except when state is -1, in which case the value in \code{RollShort}.
  
}
  

\seealso{

  trades

}

\examples{

x <- matrix(rnorm(150), ncol=3, dimnames=list(NULL, c("Bid","Ask","Close")))
(prices(x, c(1,1,0,-1,1), c(Mark="Close",Long="Ask",Short="Bid")))

}


\value{

  A matrix with columns "Price", "Roll", "Mark", "Long", "Short",
  "RollLong", and "RollShort".
  
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}