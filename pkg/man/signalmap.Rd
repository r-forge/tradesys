
\name{signalmap}

\alias{signalmap}

\title{Signals-to-States}

\description{
  
  Map signal vectors to a vector of states.
  
}

\usage{

signalmap(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, entrywins=FALSE) 

}

\arguments{
  \item{el}{logical. enter long signals.}
  \item{es}{logical. enter short signals.}
  \item{xl}{logical. exit long signals.}
  \item{xs}{logical. exit short signals.}
  \item{entrywins}{logical. see below.}
}
\details{
  
  \code{signalmap} converts entry and exit signals into a numeric vector
  of states. A signal is a logical vector and is one of four types:
  \emph{enter long}, \emph{enter short}, \emph{exit long}, and
  \emph{exit short}. All four signals taken together determine state
  \eqn{s}. 

  \eqn{s_{i}=1}{s(i)=1} when \code{el[i]} is TRUE and
  \eqn{s_{i}=-1}{s(i)=-1} when \code{es[i]} is TRUE. When both
  \code{el[i]} and \code{es[i]} are FALSE,
  \eqn{s_{i}=s_{i-1}}{s(i)=s(i-1)}. These entry rules imply the property
  of "once in, always in"; upon the first entry signal, the system's
  state vector is thereafter alternating sequences of 1's and -1's.
  
  \code{xl} and \code{xs} augment the entry rules with exit rules. Where
  \code{xl[i]} is TRUE and \eqn{s_{i}=1}, \eqn{s_{i}} and the series of
  consecutive 1's between it and the next entry signal are set to
  0. Similarly, where \code{xs} is TRUE and \eqn{s_{i}=-1}, \eqn{s_{i}}
  and the series of consecutive -1's between it and the next entry
  signal are set to 0. In words: "once out, stay out until \emph{any}
  entry signal occurs". 

  Where both an entry and exit signal conflict the exit wins unless
  \code{entrywins=TRUE}. All signal vectors must be logical and of the
  same length (although single-valued logicals are expanded). NA's are
  treated as FALSE.
  
  Here is a complete definition of the mapping computed by
  \code{signalmap}. Think of each signal quad as a four digit binary
  number, where the left-most digit represents the enter long signal,
  the next digit represents the enter short signal, and so on. 
  \tabular{lll}{
    Bin \tab Int \tab State \eqn{s_{t}}{s(t)}\cr
    \code{0000} \tab 0 \tab \eqn{s_{t-1}}{s(t-1)} \cr
    \code{0001} \tab 1 \tab \eqn{max(s_{t-1}, 0)}{max[s(t-1), 0]} \cr
    \code{0010} \tab 2 \tab \eqn{min(s_{t-1}, 0)}{min[s(t-1), 0]} \cr
    \code{0011} \tab 3 \tab 0 \cr
    \code{0100} \tab 4 \tab -1 \cr
    \code{0101} \tab 5 \tab \eqn{max(s_{t-1}, 0)}{max[s(t-1), 0]}\cr %%%
    \code{0110} \tab 6 \tab -1\cr
    \code{0111} \tab 7 \tab 0\cr %%%
    \code{1000} \tab 8 \tab 1\cr
    \code{1001} \tab 9 \tab 1\cr
    \code{1010} \tab 10\tab \eqn{min(s_{t-1}, 0)}{min[s(t-1), 0]}\cr %%%
    \code{1011} \tab 11\tab 0\cr %%%
    \code{1100} \tab 12\tab \eqn{s_{t-1}}{s(t-1)} \cr
    \code{1101} \tab 13\tab \eqn{max(s_{t-1}, 0)}{max[s(t-1), 0]} \cr
    \code{1110} \tab 14\tab \eqn{min(s_{t-1}, 0)}{min[s(t-1), 0]} \cr
    \code{1111} \tab 15\tab 0\cr
  }

  12-15 will be treated like 0-3, as it is a nonesense to have both
  \code{el[i]} and \code{es[i]} as TRUE.
  
  When \code{entrywins} is TRUE, 5 and 7 map to -1 and 10 and 11 map to
  1.
  
}

\value{
  
  A state vector of the same length as the signal vectors.
  
}

\examples{
el <- c(TRUE,FALSE)
es <- c(FALSE,FALSE)
signalmap(el, es)
el[2] <- TRUE
xl <- c(FALSE,TRUE)
signalmap(el, es, xl)
signalmap(el, es, xl, entrywins=TRUE)

el <- c(TRUE,FALSE,TRUE)
es <- c(FALSE,FALSE,FALSE)
xl <- c(FALSE,TRUE,FALSE)

signalmap(el, es, xl)
signalmap(es, el, FALSE, xl)
}
\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}