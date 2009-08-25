\name{tradesys}

\alias{tradesys}
\alias{print.tradesys}
\alias{as.list.tradesys}
\alias{$<-.tradesys}
\alias{[[<-.tradesys}
\alias{[<-.tradesys}
\alias{as.tradesys}
\alias{as.tradesys.default}

\title{Trading System Object}

\description{

  Create a "tradesys" object, a complete definition of trading system.

}

\usage{

tradesys(datavars, pricemap=c(Mark=datavars[1]),
         el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, delta=1,
         size.at=FALSE, roll.at=FALSE, percent=TRUE, 
         entrywins=FALSE, entrycond=FALSE, exprvars=NULL)

}

\arguments{

  \item{datavars}{character vector defining the system's data variable names.}
  \item{pricemap}{named character vector defining the mapping between
    price types and data variable names.}
  \item{el}{enter long signal. expression that evaluates to a logical
    vector.}
  \item{es}{enter short signal. expression that evaluates to a logical
    vector.}
  \item{xl}{exit long signal. expression that evaluates to a logical
    vector.}
  \item{xs}{exit short signal. expression that evaluates to a logical
    vector.}
  \item{delta}{numeric vector defining the relationship between changes
    in price and changes in equity.}
  \item{roll.at}{logical vector stating those times at which a roll
    trade is done.}
  \item{size.at}{logical vector stating those times at which equity is
    re-sized to \code{delta}.}
  \item{percent}{logical. defines \code{delta} as percentage or absolute
    change in price.}
  \item{entrywins,entrycond}{logical. see \code{signalmap}.}
  \item{exprvars}{a named list of quoted expressions.}

}

\details{

  A 'tradesys' object is a complete description of a trading
  system. \code{tradesys} returns its formal arguments as a named list
  with class attribute 'tradesys'. The argument values are first checked for
  consistency. A 'tradesys' object can be subsetted in the usual list way
  (the 'tradesys' class is stripped) and the \code{[.tradesys<-} and
  \code{$.tradesys<-} assignment methods call \code{tradesys} with the
  modified value. A \code{print} method prints the contents in a
  sensible manner and the \code{as.list} method and \code{as.tradesys}
  default method allow for translation to and from plain lists.
  
  \code{datavars} is a character vector defining the data variable names
  the 'tradesys' object gets evaluated with in a call to
  \code{tradesys.frame}. Any of the expressions passed to \code{tradesys}
  can refer to such data by including one or more of these names in the
  expression.
  
  \code{pricemap} specifies which data variables contain price
  information. There are five categories of price information that need
  to be mapped to data variables:
  
  \itemize{
    \item Mark
    \item Long
    \item Short
    \item RollLong
    \item RollShort
  }  
  
  These are the names of the vector passed to \code{pricemap}. The
  corresponding values are the data variable names to which they are
  mapped. The meaning of this mapping is pretty transparent. A long
  entry or short exit is assumed to trade at the price in the
  \emph{Long} variable, a short entry or long exit is assumed to trade
  at the price in the \emph{Short} variable, and unchanged periods are
  valued using the price in the \emph{Mark} variable. The vector need
  not specify all five. By default, 'Long' and 'Short' are keyed to
  'Mark', 'RollLong' to 'Long' and 'RollShort' to 'Short'. At least
  'Mark' must be defined, but a scalar may be passed, in which case all
  five are given that value. An error is raised if \code{datavars} does
  not contain a column identifier that you pass.

  \code{el}, \code{es}, \code{xl}, and \code{xs} encode the system's
  enterlong, entershort, exitlong and exitshort signals respectively.
  When \code{tradesys.frame} is called, these must be expressions that
  evaluate to a logical vector. Those logical vectors are used to
  calculate the system's states using \code{signalmap}. \code{entrywins}
  and \code{entrycond} are logical values that redefine the meaning of
  the signal-to-state mapping in certain edge cases. Consult the
  \code{signalmap} documentation for details.

  \code{delta} defines the system's leveraging scheme. When
  \code{tradesys.frame} is called it must evaluate to a numeric vector. See
  the documentation for \code{equity} for details.
  
  \code{size.at} defines the times when the effect of price change on
  equity is re-sized to delta. By default these evaluate to those times
  when there is a change in state. \code{roll.at} is optional and
  defines the times when a "roll trade" is performed. Both are
  expressions that evaluate to logical vectors. \code{percent} is a
  logical value that specifies whether delta is defined over percentage
  or absolute changes in price. All three parameters are used in
  calculating the system's equity vector. Consult documentation of
  \code{equity} for details.
  
  \dots is optional. If it is passed anything, it must be passed one or
  more quoted expressions with unique argument names. These are stored
  collectively as a list in the \code{exprvars} slot. These provide a
  convienent way to define an arbitrary number of ad-hoc "derived"
  variables. The other expressions (except the entry/exit signals) may
  refer to them by name. They must evaluate to a numeric vector. 
  
  Upon being evaluated by \code{tradesys.frame}, all of these expressions must:
  
  \itemize{
    \item successfully evaluate on \emph{data}, where \emph{data} is any
    set of numeric variables whose names are in \code{datavars} and are
    all of the same length,
    \item result in a vector whose length is equal to the length of \emph{data}
    or 1,
    \item and return its correct type (logical or numeric).
  }
  
  An expression can contain any valid function or operator (SEE README
  RE NEEDED CHANGES TO ENVIRONMENT CONSTRUCTION), but names are
  restricted to those in \code{datavars}, \code{exprvars}, the price
  variables, and the special name "index" (see \code{tradesys.frame}
  documentation for details).

}

\value{

  A tradesys object.

}

\examples{
x <- tradesys(c("Bid","Ask","Close"))

## assignment
x$pricemap <- c(Mark="Close", Long="Ask", Short="Bid")
library(TTR)
x[["el"]] <- quote(SMA(Close, 60) >= SMA(Close, 120))
x$es <- quote(SMA(Close, 60) < SMA(Close, 120))

## .. same as 'x' but all in one call
y <- tradesys(c("Bid","Ask","Close"), pricemap=c(Mark="Close",
Long="Ask", Short="Bid"), el=SMA(Close, 60) >= SMA(Close, 120), es=SMA(Close, 60) < SMA(Close, 120))
stopifnot(identical(x, y))

## .. same as x but via coercion of a list
stopifnot(identical(x, as.tradesys(as.list(x))))

## extraction
x$el
x$es
x[4:8]
x[["el"]]
x$pricemap

## coercion
as.list(x)
}

\author{Robert Sams \email{robert@sanctumfi.com}}

\keyword{math}