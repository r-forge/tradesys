\name{statemap}

\alias{phasemap}
\alias{statemap}

\title{States and Phases}

\description{
  Map states to phases and vice-versa.
}

\usage{
phasemap(states)
statemap(phases)
}

\arguments{
  \item{states}{numeric vector of states.}
  \item{phases}{character vector of phases.}
}

\details{
  A tradings system's states is a numeric vector consisting of 1's, 0's
  and -1's, corresponding to market positions "long", "flat" and "short"
  respectively.  
  
  The transition of one state to the next is a "phase" and each
  of the nine possible configurations for state pairs
  \eqn{s_{i},s_{i-1}}{s(i),s(i-1)} is mapped to a phase \eqn{h_{i}}{h(i)} using
  the following rules:
  \tabular{lllll}{
    EL \tab if \tab \eqn{s_{i} = 1}{s(i) = 1} \tab
    \eqn{\wedge}{\&} \tab \eqn{s_{i-1} \neq 1}{s(i-1) != 1}\cr
    ES \tab if \tab \eqn{s_{i} = -1}{s(i) = -1}\tab
    \eqn{\wedge}{\&}\tab \eqn{s_{t-1} \neq -1}{s(i-1) != -1}\cr
    XL \tab if \tab \eqn{s_{i} = 0}{s(i) = 0}
    \tab \eqn{\wedge}{\&} \tab \eqn{s_{i-1} = 1}{s(i-1) = 1}\cr
    XS \tab if \tab \eqn{s_{i} = 0}{s(i) = 0}
    \tab \eqn{\wedge}{\&} \tab \eqn{s_{i-1} = -1}{s(i-1) = -1}\cr
    UC \tab if\tab \eqn{s_{i} = s_{i-1}}{s(i) = s(i-1)} \tab \tab
  }
  \code{phasemap} calculates each \eqn{h_{i}} from the state vector and
  returns a vector of these two-character phase codes.
  
  \code{statemap} converts a vector of phases into a numeric vector of
  states. The phase-to-state mapping is:
  \tabular{rll}{
    1                                    \tab if \tab EL \cr
    -1                                   \tab if \tab ES \cr
    \eqn{min(0, s_{i-1})}{min[0, s(i-1)]} \tab if \tab XL \cr
    \eqn{max(0, s_{i-1})}{max[0, s(i-1)]} \tab if \tab XS \cr
    0                                    \tab if \tab XA \cr
    \eqn{s_{i-1}}{s(i-1)}                \tab if \tab UC
  }
}
\value{

  A numeric (\code{statemap}) or character (\code{phasemap}) vector.

}
\examples{
## statemap is invertable
statemap(p <- phasemap(s <- c(0,0,1,1,1,1,-1,-1,-1,0,0))) == s
## phasemap is not always invertable.. for example
q <- p
q[4] <- "EL"
phasemap(statemap(q)) == q
}
\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}