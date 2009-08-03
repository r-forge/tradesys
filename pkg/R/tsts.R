tsts <- function(data, order.by=index(data), pricecols=1, el=FALSE, es=FALSE, 
                 xl=FALSE, xs=FALSE, delta=1, size.at=as.logical(c(St[1], diff(St))),
                 roll.at=FALSE, exprcols=NULL, percent=TRUE, entrywins=FALSE,
                 entrycond=FALSE){
  ## Process data args
  if(length(order.by) != length(unique(order.by)))
    stop("all order.by must be unique.")
  if(length(order.by) != nrow(data))
    stop("length(order.by) must equal nrow(data)")
  data <- as.matrix(data)
  if(length(colnames(data)) != unique(length(colnames(data))))
    stop("data must have unique column names.")
  l <- list(pricecols=processPriceCols(data, pricecols))
  ## Process expression args
  l$el <- substitute(el)
  l$es <- substitute(es)
  l$xl <- substitute(xl)
  l$xs <- substitute(xs)
  l$delta <- substitute(delta)
  l$size.at <- substitute(size.at)
  l$roll.at <- substitute(roll.at)
  l$exprcols <- substitute(exprcols)
  ## Process logical args
  if(!is.logical(percent)){
    warning("percent must be logical.. using FALSE.")
    percent <- FALSE
  }
  if(!is.logical(entrywins)){
    warning("entrywins must be logical.. using FALSE.")
    entrywins <- FALSE
  }
  if(!is.logical(entrycond)){
    warning("entrycond must be logical.. using FALSE.")
    entrycond <- FALSE
  }
  l$percent <- percent
  l$entrywins <- entrywins
  l$entrycond <- entrycond
  ## Evaluate exprcols
  if(!is.null(l$exprcols)){
    Frame <- as.list(as.data.frame(data))
    Frame$index <- order.by
    m <- eval(l$exprcols, Frame)
    if(is.list(m))
      data <- cbind(data, do.call("cbind", m))
    else
      data <- cbind(data, m)
  }
  ## Evaluate signals
  Frame <- as.list(as.data.frame(data))
  Frame$index <- order.by
  St <- do.call("signalmap", lapply(c(l["el"], l["es"], l["xl"], l["xs"], l["entrycond"], l["entrywins"]), eval, Frame))
  ## Evaluate delta, roll.at, size.at
  Frame <- as.list(as.data.frame(data))
  Frame$St <- St
  Frame$index <- order.by
  delta <- eval(l$delta, Frame)
  size.at <- eval(l$size.at, Frame)
  roll.at <- eval(l$roll.at, Frame)
  ## Calculate equity
  Equity <- equity(prices(data, l$pricecols), St, delta, size.at, roll.at, l$percent)[, "Equity"]
  data <- cbind(St, Equity, data)
  attr(data, "index") <- order.by
  attr(data, "tstsp") <- l
  class(data) <- "tsts"
  data
}

###
### METHODS CLASS 'tsts'
###

"[.tsts" <- function(x, i=NULL, j=NULL, ..., drop=TRUE){
  if(is.null(i))
    i <- 1:nrow(x)
  if(is.null(j))
    j <- 1:ncol(x)
  as.matrix(x)[i, j, drop=drop]
}

print.tsts <- function(x, ...){
  print(as.zoo(x, ...))
}

index.tsts <- function(x, ...){
  attr(x, "index")
}

"index<-.tsts" <- function(x, ..., value){
  if(length(value) != length(index(x)))
    stop("index(x) and value must be the same length.")
  if(class(index(x))[1] != class(value)[1])
    warning("index(x) and value are not of the same class.")
  attr(x, "index") <- value
  x
}

start.tsts <- function(x, ...){
  attr(x, "index")[1]
}

end.tsts <- function(x, ...){
  attr(x, "index")[nrow(x)]
}

head.tsts <- function(x, ...){
  head(as.zoo(x), ...)
}

tail.tsts <- function(x, ...){
  tail(as.zoo(x), ...)
}

as.matrix.tsts <- function(x, ...){
  rownames(x) <- format(attr(x, "index"))
  attr(x, "index") <- NULL
  attr(x, "tstsp") <- NULL
  class(x) <- "matrix"
  x
}

as.zoo.tsts <- function(x, ...){
  zoo(as.matrix.tsts(x), order.by=attr(x, "index"))
}
