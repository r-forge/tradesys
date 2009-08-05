tradesys <- function(datacols, pricecols, el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, 
                    delta=1, size.at=as.logical(c(St[1], diff(St))),
                    roll.at=FALSE, formulae=NULL, percent=TRUE, entrywins=FALSE,
                    entrycond=FALSE){
  l <- list(pricecols=pricecols)
  l$el <- substitute(el)
  l$es <- substitute(es)
  l$xl <- substitute(xl)
  l$xs <- substitute(xs)
  l$delta <- substitute(delta)
  l$size.at <- substitute(size.at)
  l$roll.at <- substitute(roll.at)
  l$formulae <- substitute(formulae)
  l$percent <- percent
  l$entrywins <- entrywins
  l$entrycond <- entrycond
  l
}

tsys <- function(x){
  if(!is.tsts(x))
    return(invisible(NULL))
  attr(x, "tsys")
}

"tsys<-" <- function(x, value){
  if(is.tsts(x)){
    attr(x, "tsys") <- value
    return(as.tsts.default(x))
  }
  ARGS <- value
  ARGS$data <- x
  ARGS$order.by <- index(x)
  do.call("tsts", ARGS)
}
