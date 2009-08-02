tsts <- function(data, order.by=index(data), states=NULL, pricecols=1, exprcols=NULL, signals=NULL, 
                 delta=1, size.at=statechg(St), roll.at=FALSE, percent=TRUE, entrywins=FALSE, entrycond=FALSE){
  if(length(order.by) != length(unique(order.by)))
    stop("all order.by must be unique.")
  if(length(order.by) != nrow(data))
    stop("length(order.by) must equal nrow(data)")
  data <- as.matrix(data)
  if(length(colnames(data)) != unique(length(colnames(data))))
    stop("data must have unique column names.")
  if(!is.null(states)){
    if(nrow(data) %% length(states) != 0)
      stop("length(states) must be a multiple of nrow(data)")
    if(!any(states %in% c(1,0,-1)))
      stop("states must be a numeric vector containing only 1, 0, and -1 values")
  }else{
    if(missing(signals)){
      message("neither states or signals specified.. using states=0")
      states <- 0
    }
  }
  l <- list(pricecols=processPriceCols(data, pricecols))
  l <- c(l, list(exprcols=exprcols))
  if(!is.null(signals)){
    if(!is.list(signals)){
      message("signals must be a list.. setting to NULL.")
      signals <- NULL
    }else{
      if(any(!names(signals) %in% c("el","es","xl","xs")))
        stop("only el, es, xl and xs are valid signal names")
      signals <- replace(list(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE),
                         match(names(signals), c("el","es","xl","xs")), signals)
    }
  }
  l <- c(l, list(signals=signals))
  l$delta <- as.expression(substitute(delta))
  l$size.at <- as.expression(substitute(size.at))
  l$roll.at <- as.expression(substitute(roll.at))
  if(!is.logical(percent)){
    warning("percent must be logical.. using FALSE.")
    l$percent <- FALSE
  }else{
    l$percent <- percent
  }
  if(!is.logical(entrywins)){
    warning("entrywins must be logical.. using FALSE.")
    l$entrywins <- FALSE
  }else{
    l$entrywins <- entrywins
  }
  if(!is.logical(entrycond)){
    warning("entrycond must be logical.. using FALSE.")
    l$entrycond <- FALSE
  }else{
    l$entrycond <- entrycond
  }
  ## Evaluate exprcols
  if(!is.null(l$exprcols)){
    m <- eval(l$exprcols, as.list(as.data.frame(data)))
    if(is.list(m))
      data <- cbind(data, do.call("cbind", m))
    else
      data <- cbind(data, m)
  }
  ## Evaluate signals
  if(is.null(states)){
    s <- signalmap(eval(l$signals$el, as.list(as.data.frame(data))),
                   eval(l$signals$es, as.list(as.data.frame(data))),
                   eval(l$signals$xl, as.list(as.data.frame(data))),
                   eval(l$signals$xs, as.list(as.data.frame(data))),
                   l$entrycond, l$entrywins)
    data <- cbind(data, St=s)
  }else{
    data <- cbind(data, St=states)
  }
  attr(data, "index") <- order.by
  attr(data, "tstsp") <- l
  class(data) <- "tsts"
  ## Evaluate delta, roll.at, size.at
  ##e <- equity(prices(data), s, attr(data, "tstsp")$delta, size.at(data), roll.at(data), attr(data, "tstsp")$percent=TRUE)
  ##cbind(data, Equity=e)
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
p}

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
