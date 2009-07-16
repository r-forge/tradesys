
\name{signals}

\alias{signals}

\title{Signals-to-States}

\description{
  Convert signal vectors to a vector of states.
}

\usage{
signals(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, entrycond=FALSE, entrywins=FALSE) 
}

\arguments{
  \item{el}{boolean. enter long signals.}
  \item{es}{boolean. enter short signals.}
  \item{xl}{boolean. exit long signals.}
  \item{xs}{boolean. exit short signals.}
  \item{entrycond}{logical. see below.}
  \item{entrywins}{logical. see below.}
}
\details{
  
  \code{signals} converts entry and exit signals into a numeric vector
  of states. A signal is a logical value and is one of four types:
  \emph{enter long}, \emph{enter short}, \emph{exit long}, and
  \emph{exit short}. All four signals taken together determine state
  \eqn{s}. 

  \eqn{s_{i}=1}{s(i)=1} when \code{el[i]} is TRUE and
  \eqn{s_{i}=-1}{s(i)=-1} when \code{es[i]} is TRUE. When both
  \code{el[i]} and \code{es[i]} are FALSE,
  \eqn{s_{i}=s_{i-1}}{s(i)=s(i-1)}. An error is raised if \code{el[i]}
  and \code{es[i]} are both TRUE. These entry rules imply the property
  of "once in, always in"; upon the first entry signal, the system's
  state vector is thereafter alternating sequences of 1's and -1's.
  
  \code{xl} and \code{xs} augment the entry rules with exit rules. Where
  \code{xl[i]} is TRUE and \eqn{s_{i}=1}, \eqn{s_{i}} and the unbroken
  series of 1's between it and the next entry signal are set to
  0. Similarly, where \code{xs} is TRUE and \eqn{s_{i}=-1},
  \eqn{s_{i}} and the unbroken series of -1's between it and the next
  entry signal are set to 0. In words: "once out, stay out until
  \emph{any} entry signal occurs". The state vector then has no
  particular form. 

  As the above decription implies, entry signals are by default
  unconditional. If \code{entrycond} is TRUE, entry rules become
  conditional: "once out, stay out until the \emph{opposite} entry
  signal occurs." The state vector is then alternating sequences of
  1's and -1's (possibly) punctuated by 0's. 
  
  Where both an entry and exit signal conflict the exit wins unless
  \code{entrywins=TRUE}. All signal vectors must be logical and of the
  same length (although single-valued logicals are expanded). NA's are
  treated as FALSE.
  
  Here is a complete definition of the mapping computed by
  \code{signals}. Think of each signal quad as a four digit binary
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
    \code{1100} \tab 12\tab INVALID\cr
    \code{1101} \tab 13\tab INVALID\cr
    \code{1110} \tab 14\tab INVALID\cr
    \code{1111} \tab 15\tab INVALID\cr
  }
  When \code{entrywins} is TRUE, binary 5 and 7 map to -1 and binary 10
  and 11 map to 1. When \code{entrycond} is TRUE, the signals themselves
  are altered in the code before performing the above mapping. When
  \code{xl[i]} is TRUE, \code{el[i:j]} is set to FALSE where \code{j} is
  the offset of the first TRUE \code{es} after \code{i}. Symmetrically,
  when \code{xs[i]} is TRUE, \code{es[i:j]} is set to FALSE where
  \code{j} is the offset of the first TRUE \code{el} after \code{i}.

  
  
}

\value{
  A state vector of the same length as the signal vectors.
}

\examples{
signals(TRUE)
}

\author{Robert Sams \email{robert@sanctumfi.com}}
\keyword{math}