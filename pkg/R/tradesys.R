tradesys <- function(prices, rollout=prices, rollin=prices,
                     states=0, roll.at=FALSE, size.at=FALSE, delta=1, 
                     equity=ror(cbind(prices,rollout,rollin), states,
                       roll.at, size.at, delta), ...){
  l <- list(prices=substitute(prices),
            rollout=substitute(rollout),
            rollin=substitute(rollin),
            states=substitute(states),
            roll.at=substitute(roll.at),
            size.at=substitute(size.at),
            delta=substitute(delta),
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

"[.tradesys" <- function(x, i){
  y <- as.list.tradesys(x)[i]
  FNames <- names(formals(tradesys))
  if(!all(FNames[-length(FNames)] %in% names(y)))
    return(y)
  as.tradesys(y)
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
