\name{prices}

\alias{prices}

\title{Price Context}

\description{
  
  \code{prices} returns a common price matrix from some data matrix and
  states vector.
  
}

\usage{

prices(x, states, pricemap, roll.at=FALSE)

}

\arguments{
  
  \item{x}{numeric matrix, object coerciable via \code{as.matrix} or of
    class 'tsts'.}
  \item{states}{numeric vector of states.}
  \item{pricemap}{named character vector defining the mapping between
    price types and data variable names.}
  \item{roll.at}{logical. must be multiple of \code{nrow(x)}.}

}

\details{
  
  When \code{x} is class 'tsts', any values passed to \code{states},
  \code{pricemap} and \code{roll.at} are ignored and replaced with
  \code{x[, "St"]}, \code{tsys(x)$pricemap}, \code{tsys(x)$roll.at}.

  \code{pricemap} specifies which variables in \code{x} contain which
  of the five types of price information:
  
  \itemize{
    \item Mark
    \item Long
    \item Short
    \item RollLong
    \item RollShort
  }  
  
  These must be the names of the vector passed to \code{pricemap}. The
  corresponding values are the column names in \code{x} to which the
  price types are mapped. The vector need not specify all five. By
  default, 'Long' and 'Short' are keyed to 'Mark', 'RollLong' to 'Long'
  and 'RollShort' to 'Short'. At least 'Mark' must be defined, but a
  scalar may be passed, in which case all five are given that value. An
  error is raised if \code{colnames(x)} does not contain a column
  identifier that you pass.
  
  The function returns a matrix of seven columns: \code{"Price"},
  \code{"Roll"}, \code{"Mark"}, \code{"Long"}, \code{"Short"}, \code{"RollLong"},
  and \code{"RollShort"} prices. Columns 3-7 simply extract the relevant
  columns from \code{x} on the basis of the \code{pricemap}.

  The \code{"Price"} column is choosen in such a way that times where
  there is a long entry or short exit are assigned the corresponding
  price in \code{"Long"} column. Times where there is a short entry or
  long exit are assigned the corresponding price in the \code{"Short"}
  column, and unchanged times are valued using the price in the
  \code{Mark} column.

  The \code{Roll} column contains the value in \code{RollLong},
  except when state is -1, in which case the value in \code{RollShort}.

  The parameter \code{roll.at} is a logical vector whose length must be
  a multiple of \code{nrow(x)}. For all those times where \code{roll.at}
  is TRUE and \code{states} is 1, the value in \code{"Short"} is placed
  in \code{"Price"}. For all those times where \code{roll.at} is TRUE
  and \code{states} is -1, the value in \code{"Long"} is placed in
  \code{"Price"}. The \code{"Price"} and \code{"Roll"} columns therefore
  represent...  
  
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