tradesys <- function(states=0, size.at=FALSE, roll.at=FALSE, delta=1, prices, 
                     equity=ror(prices, states, roll.at, delta, size.at), ...){
  l <- list(states=substitute(states),
            size.at=substitute(size.at),
            roll.at=substitute(roll.at),
            delta=substitute(delta),
            prices=substitute(prices),
            equity=substitute(equity))
  Dots <- list(...)
  if(any(duplicated(names(Dots))))
    stop("Expressions passed to ... must be (uniquely) tagged.")
  l <- c(l, Dots)
  class(l) <- "tradesys"
  l
}

##
## Methods
##

print.tradesys <- function(x, ...){
  print(as.list(x, ...))
}

"$<-.tradesys" <- function(x, i=NULL, value){
  y <- as.list.tradesys(x)
  if(is.null(i))
    y <- value
  else
    y[[i]] <- value
  class(y) <- "tradesys"
  y
}

"[[<-.tradesys" <- function(x, i, value){
  "$<-.tradesys"(x, i, value)
}

"[<-.tradesys" <- function(x, i, value){
  "$<-.tradesys"(x, i, value)
}

as.list.tradesys <- function(x, ...){
  class(x) <- NULL
  x
}

##
## New Generic
##

as.tradesys <- function(x, ...){
  UseMethod("as.tradesys", x)
}

as.tradesys.default <- function(x, ...){
  y <- tradesys()
  if(!is.list(x))
    stop("the default method coerces only lists.")
  if(any(duplicated(names(x))))
    stop("names(x) must be unique.")
  for(i in 1:length(x))
    y[[names(x)[i]]] <- x[[i]]
  y
}
