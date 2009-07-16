tradesys <- function(data, el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, entrycond=FALSE, entrywins=FALSE, makecols=NULL, ...){
  d <- as.matrix(data)
  l <- as.list(as.data.frame(d))
  makecols <- eval(substitute(makecols), l)
  if(!is.null(makecols)){
    if(any(unlist(lapply(makecols, function(x) !length(x) %in% c(1, nrow(d))))))
      stop("expressions in makecols list must evaluate to vector of length nrow(data) or 1")
    if(length(unique(names(makecols))) != length(makecols))
      stop("names(makecols) must be unique")
    if(any(names(makecols) == ""))
      stop("names(makecols) cannot be ''")
    if(any(names(makecols) %in% c(colnames(d),"states","States")))
      stop("names(makecols) cannot be 'states' or in colnames(data)")
    d <- cbind(d, do.call("cbind", makecols))
  }
  s <- signals(eval(substitute(el), l), eval(substitute(es), l), eval(substitute(xl), l), eval(substitute(xs), l), entrycond, entrywins)
  x <- tsts(d, order.by=index(data), states=s, ...)
  attr(x, "tradesys")$el <- substitute(el)
  attr(x, "tradesys")$es <- substitute(es)
  attr(x, "tradesys")$xl <- substitute(xl)
  attr(x, "tradesys")$xs <- substitute(xs)
  attr(x, "tradesys")$entrycond <- entrycond
  attr(x, "tradesys")$entrywins <- entrywins
  attr(x, "tradesys")$makecols <- makecols
  attr(x, "tradesys")$coreattr <- attributes(data)
  class(x) <- c("tradesys","tsts")
  x
}

##
## METHODS CLASS tradesys
##

coredata.tradesys <- function(x, ...){
  y <- x[, attr(x, "tradesys")$coreattr$dimnames[[2]]]
  attributes(y) <- attr(x, "tradesys")$coreattr
  y
}

"coredata<-.tradesys" <- function(x, ..., value){
  ## !! CODE ME !!
}

as.tsts.tradesys <- function(x, ...){
  attr(x, "tradesys") <- NULL
  class(x) <- "tsts"
  x
}
