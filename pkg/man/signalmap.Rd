\name{signalmap}

\alias{signalmap}
\alias{statemap}
\alias{phasemap}

\title{Translating between Signals, States and Phases}

\description{
  
  \code{signalmap} returns a vector of states given four signal vectors.
  
  \code{statemap} returns a vector of states given a vector of phases.
  
  \code{phasemap} returns a vector of phases given a vector of states.
}

\usage{

signalmap(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, entrywins=FALSE,
          retval=c("states","phases","binary")) 

statemap(phases)

phasemap(states)



}

\arguments{
  \item{el}{logical. enter long signals.}
  \item{es}{logical. enter short signals.}
  \item{xl}{logical. exit long signals.}
  \item{xs}{logical. exit short signals.}
  \item{entrywins}{logical. see below.}
  \item{retval}{character. see below.}
  \item{states}{numeric vector of states.}
  \item{phases}{character vector of phases.}
}
\details{
  
  \code{signalmap} converts entry and exit signals into a numeric vector
  of states. A signal is a logical vector and is one of four types:
  \emph{enter long}, \emph{enter short}, \emph{exit long}, and
  \emph{exit short}. All four signals taken together make up a 4-bit
  binary number between 0 and 15 (e.g., \code{el=TRUE} with the other
  three signals \code{FALSE} is 8). Six of these are called
  \emph{phases} and are special:
  
  \tabular{lll}{
    Signals \tab Phase  \tab English \cr
    \code{0000} \tab 0 \tab Unchanged    \cr
    \code{0001} \tab 1 \tab Exit Short   \cr
    \code{0010} \tab 2 \tab Exit Long    \cr
    \code{0011} \tab 3 \tab Exit All     \cr
    \code{0100} \tab 4 \tab Enter Short  \cr
    \code{1000} \tab 8 \tab Enter Long   
  }
  
  A phase specifies the transition from one state to the
  next. \code{statemap} recursively calculates a vector of states given a
  vector of phases using the following rules:
  
  \tabular{lrl}{
    \eqn{s_{i} = s_{i-1}}{s(i) = s(i-1)} \tab if \tab \eqn{h_{i} = 0}{h(i) = 0} \cr
    \eqn{s_{i} = max(0, s_{i-1})}{s(i) = max[0, s(i-1)]} \tab if \tab \eqn{h_{i} = 1}{h(i) = 1}\cr
    \eqn{s_{i} = min(0, s_{i-1})}{s(i) = min[0, s(i-1)]} \tab if \tab \eqn{h_{i} = 2}{h(i) = 2}\cr
    \eqn{s_{i} = 0}{s(i) = 0} \tab if \tab \eqn{h_{i} = 3}{h(i) = 3} \cr
    \eqn{s_{i} = -1}{s(i) = -1} \tab if \tab \eqn{h_{i} = 4}{h(i) = 4} \cr
    \eqn{s_{i} = 1}{s(i) = 1} \tab if \tab \eqn{h_{i} = 8}{h(i) = 8} 
  }
  
  \code{signalmap} calls \code{statemap} in calculating the state
  vector, but it first reduces the ten non-phase combinations to a phase
  using the following mapping:
  
  \tabular{l}{
    \code{0101 [05] --> 0001 [1]}\cr
    \code{0110 [06] --> 0100 [4]}\cr
    \code{0111 [07] --> 0011 [3]}\cr
    \code{1001 [09] --> 1000 [8]}\cr
    \code{1010 [10] --> 0010 [2]}\cr
    \code{1011 [11] --> 0011 [3]}\cr
    \code{1100 [12] --> 0000 [0]}\cr
    \code{1101 [13] --> 0001 [1]}\cr
    \code{1110 [14] --> 0010 [2]}\cr
    \code{1111 [15] --> 0011 [3]}
  }
  
  If \code{entrywins=TRUE} the mapping is altered so that conflicts
  between an entry and an exit resolve into the entry, instead of the
  exit (which is the default behavior above):
  
  \tabular{l}{
    \code{0101 [05] --> 0100 [4]}\cr
    \code{0110 [06] --> 0100 [4]}\cr
    \code{0111 [07] --> 0100 [4]}\cr
    \code{1001 [09] --> 1000 [8]}\cr
    \code{1010 [10] --> 1000 [8]}\cr
    \code{1011 [11] --> 1000 [8]}\cr
    \code{1100 [12] --> 0000 [0]}\cr
    \code{1101 [13] --> 0001 [1]}\cr
    \code{1110 [14] --> 0010 [2]}\cr
    \code{1111 [15] --> 0011 [3]}
  }
  
  \code{phasemap} does the opposite of \code{statemap}, taking a vector of
  states and returning a vector of phases. If \code{retval="phases"},
  \code{signalmap} will skip the state calculation and return a vector
  of phases instead. If \code{retval="binary"}, the signals are simply
  converted into their 4-bit integer representation and returned.
  
}

\value{
  
  A state vector of the same length as the signal vectors.
  
}

\examples{

## statemap is invertable
statemap(p <- phasemap(s <- c(0,0,1,1,1,1,-1,-1,-1,0,0))) == s
## phasemap is not always invertable.. for example
q <- p
q[4] <- 8
phasemap(statemap(q)) == q

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