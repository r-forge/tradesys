\name{coredata}

\alias{coredata.tsts}
\alias{coredata<-.tsts}

\title{Extracting/Replacing the Core Data of Trading System Time Series}

\description{
  Methods for extracting and replacing the core time series data from
  \code{tsts} objects.
}

\usage{
\method{coredata}{tsts}(x, \dots)
\method{coredata}{tsts}(x) <- value
}

\arguments{
  \item{x}{a \code{tsts} object.}
  \item{value}{numeric vector or matrix.}
  \item{\dots}{ignored.}
}

\details{
  \code{coredata} returns the exact data originally passed to the
  \code{tsts} constructor, or assigned via the \code{coredata}
  assignment method.

  The assignment method is more involved. If \code{nrow(value)} equals
  \code{nrow(x)}, then the index of \code{x} is preserved but
  \code{value} replaces the data. If the rows are not equal,
  \code{index(value)} becomes the new index. The column structure need
  to be the same but must be consistent with attributes \code{pricecols}
  and \code{exprcols}, and \code{exprsigs}.

  The assigment function is called by \code{addel}, \code{addes},
  \code{addxl}, \code{addes}, and \code{addcol} because it evaluates
  the tsts object's expression attributes as a side effect. The sequence
  by which this is done is as follows:
  \enumerate{
    \item \code{attributes(value)} is assigned to the \code{coreattr}
    attribute of \code{x}.
    \item Expressions in the \code{exprcols} attribute are evaluated in
    order and cbinded to \code{value} with their list names as column names.
    \item Expressions in the \code{exprsigs} attribute are evaluated and
    the states vector is computed from them using \code{signals}. Result
    is cbinded to \code{value} with column name "St".
    \item \code{tsts} and class attribute to the \code{value} and returned.
  }
}

\value{
  A tsts object.
}

\examples{
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}