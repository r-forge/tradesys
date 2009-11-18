\name{splice}

\alias{splice}
\alias{splooth}

\title{Splice and Smooth a Timeseries}

\description{

  \code{splice} will splice a timeseries.
  
  \code{splooth} will smooth a spliced timeseries.
  
}

\usage{

splice(x, at)

splooth(x, at, method=c("diff","ratio","wavg"))

}

\arguments{
  
  \item{x}{numeric matrix. see below.}

  \item{at}{logical vector indicating splice times.}
  
  \item{method}{character. "diff", "ratio", or "wavg"}
  
}

\details{
  
  Time series splicing is required when we want to construct a series
  out of contiguous time windows across a set of separate series. (For
  example, constructing a futures sequence from an ordered set of
  specific contract expiries.) The matrix of time series from which to
  take the data is called the \emph{splicing} series. The time series
  returned is called the \emph{spliced} series.

  \code{x} is the splicing series and must be coercible by
  \code{as.matrix}. Let \eqn{X_{n}^{c}}{X[n,c]} be the \eqn{n \times
  c}{n by c} matrix passed to \code{x} and \eqn{w_{1}, w_{2}, ...,
  w_{m}}{w[1], w[2], ..., w[m]} be the \eqn{m} members of
  \code{which(at)}. \code{splice} returns the spliced \eqn{n \times d}{n
  by d} matrix \eqn{Y_{n}^{d}}{Y[n,d]}, where \eqn{d = max(m - c + 1, 1)}.

  The spliced matrix is obtained by concatenating a vector diagonal of
  \eqn{X_{t}^{c}}{Y[t,c]} defined as:
  
  \deqn{
    Y_{t}^{j} = X_{t}^{f(t, j)}
  }{Y[t,j] = X[t,f(t,j)]}
  
  where \eqn{t} ranges over all values between \eqn{w_{j}}{w[j]} and \eqn{w_{j
  + c} - 1}{w[j+c] - 1} (\eqn{w_{j}}{w[j]} and \eqn{n} when \eqn{j = d}), and
  
  \deqn{
    f(t, j) = \sum_{i=w_{j}}^{t}a_{t}
  }{f(t,j) = a[j] + a[j+1] + ... + a[t]}

  , which is the column-choosing diagonal function, where
  \eqn{a_{i}}{a[i]} is the \eqn{i}-th value in \code{at}. In words: as
  we move from row \eqn{w_{j}}{w[j]} forward we take values from the
  first column of \eqn{X} until we hit another splice time
  \eqn{w_{j+1}}{w[j+1]}, when we start taking values from the second
  column, and so on until we finish taking values from the last column
  of \eqn{X}, the last value being row
  \eqn{w_{j+c}-1}{w[j+c]-1}. Now we have a column of \eqn{Y}. Repeat
  the process, but this time starting with \eqn{w_{j+1}}{w[j+1]}. Keep
  going until we have all \eqn{d} columns.
  
  If \eqn{Y_{t}}{Y[t]} is a spliced timeseries and \eqn{t} is a splice
  time, then the change between observations \eqn{Y_{t}}{Y[t]} and
  \eqn{Y_{t+1}}{Y[t+1]} contains a jump due solely to the fact that the
  two observations were originally obtained from different series. These
  jumps generally distort analysis of the spliced
  series. \emph{Sploothing} is the technique of smoothing out these
  jumps to remove the distortion. \code{splooth} is passed a spliced
  matrix to \code{x} and the splice times used in creating it are passed
  a logical vector to \code{at}. \code{method} defines the splicing
  technique to use.
  
  Three different sploothing methods are supported. Each method adjusts
  the series \code{x[, i]} using data contained in series \code{x[, i +
    1]}. A matrix of identical dimension but with sploothed values is
  returned. (The right-most column of \code{x} is left unchanged.)

  When \code{method="diff"} the \emph{difference method} is used. The
  sploothed series \eqn{\hat{Y}}{Z} is calculated by adjusting each
  observation with this equation:

  \deqn{
    \hat{Y}_{t}^{j} = Y_{t}^{j} - \sum_{i=n}^{t}(Y_{i}^{j} - Y_{i}^{j+1})a_{i}
  }{Z[t,j] = Y[t,j] - (Y[n,j] - Y[n,j+1])a[n] + ... + (Y[t,j] - Y[t,j+1])a[t]}

  where \eqn{n} is \code{nrow(x)}. The observations after the last
  splice time are unchanged, but as we move back in time the difference
  between \eqn{Y_{t}^{j}}{Y[t,j]} and \eqn{Y_{t}^{j+1}}{Y[t,j+1]} at
  splice time \eqn{t} is cumulatively subtracted from the series. Note
  that if the mean jump is meaningfully different from zero, a trend
  will be infused into the sploothed series. It is also possible for
  this method to cause the series to change sign.
  
  The \emph{ratio method} is used when \code{method="ratio"}. It is 
  calculated as:

  \deqn{
    \hat{Y}_{t}^{j} = Y_{t}^{j}\prod_{i=n}^{t}\left[(Y_{i}^{j} / Y_{i}^{j+1} - 1) a_{i} + 1\right]
  }{Z[t,j] = Y[t,j]((Y[n,j] / Y[n,j+1] - 1)a[n] + 1) ... ((Y[t,j] / Y[t,j+1] - 1)a[t] + 1)}
  
  The same proviso above about trend applies to this technique, although
  the sign will never change. All observations must be greater than zero
  or an error will be raised. 

  The \emph{weighted average method} is used when
  \code{method="wavg"}. It is calculated as:

  \deqn{
    \hat{Y}_{t}^{j} = Y_{t}^{j}\frac{k - t}{k - i} + Y_{t}^{j+1}\frac{t - j}{k - j}
  }{Z[t,j] = Y[t,j](k-t)/(k-1) + Y[t,j+1](t-j)/(k-j)}

  where \eqn{k} is the next splice time after \eqn{t}, and \eqn{j} is
  the last splice time prior to \eqn{t}. This method takes a weighted
  average of the two series at each point in time, instead of
  back-adjusting the jump observed at splice time only. The closer
  \eqn{t} gets to the splice time, the more weight is given to
  \eqn{Y_{t}^{j+1}}{Y[t,j+1]}. The same proviso holds regarding trend.
  
}

% \examples{
% }

\keyword{math}

\author{Robert Sams \email{robert@sanctumfi.com}}








