tsts <- function(data, order.by=index(data), pricecols=colnames(data)[1], el=FALSE, es=FALSE, 
                 xl=FALSE, xs=FALSE, delta=1, size.at=as.logical(c(St[1], diff(St))),
                 roll.at=FALSE, exprcols=NULL, percent=TRUE, entrywins=FALSE,
                 entrycond=FALSE){
  ## Process data and index args
  if(length(order.by) != length(unique(order.by)))
    stop("all order.by must be unique.")
  if(!is.matrix(data))
    data <- matrix(data, dimnames=list(NULL, "Price"))
  data <- as.matrix(data)
  if(is.null(colnames(data)))
    stop("colnames(data) cannot be NULL")
  if(any(duplicated(colnames(data))))
    stop("data must have unique column names.")
  if(length(order.by) != nrow(data))
    stop("length(order.by) must equal nrow(data)")
  ## Process exprcols arg (we expect a named list of calls)
  l <- list(exprcols=exprcols)
  if(!is.null(exprcols)){
    if(!is.list(exprcols) | is.null(names(exprcols)) | any(duplicated(names(exprcols))) | any(names(exprcols) %in% colnames(data)))
      stop("exprcols must be a list with names that are unique and not in colnames(data)")
    if(any(unlist(lapply(exprcols, class)) != "call"))
      stop("exprcols must be a list of call objects")
    data <- cbind(data, do.call("cbind", lapply(exprcols, eval, as.list(as.data.frame(data)))))
  }
  ## Process pricecols arg
  if(!is.vector(pricecols) | !is.character(pricecols))
    stop("pricecols must be a character vector of column names.")
  pricecols <- pricecols[1:min(length(pricecols), 5)]
  if(length(pricecols) == 1) ## everyone gets what's passed
    names(pricecols) <- NULL
  if(is.null(names(pricecols))) ## order determines mapping
    names(pricecols) <- c("Mark","Long","Short","RollLong","RollShort")[1:length(pricecols)]
  if(any(!names(pricecols) %in% c("Mark","Long","Short","RollLong","RollShort")))
    stop("All names(pricecols) must be among 'Mark','Long','Short','RollLong','RollShort'")
  if(any(!pricecols %in% colnames(data)))
    stop("all pricecols must be in colnames(data) or names(exprcols)")
  l$pricecols <- c(pricecols["Mark"], pricecols["Long"], pricecols["Short"], pricecols["RollLong"], pricecols["RollShort"])
  names(l$pricecols) <- c("Mark","Long","Short","RollLong","RollShort")
  if(is.na(l$pricecols["Mark"]))
    stop("pricecols must be passed a value for 'Mark'")
  if(is.na(l$pricecols["Long"]))
    l$pricecols["Long"] <- l$pricecols["Mark"]
  if(is.na(l$pricecols["Short"]))
    l$pricecols["Short"] <- l$pricecols["Mark"]
  if(is.na(l$pricecols["RollShort"]))
    l$pricecols["RollShort"] <- l$pricecols["Short"]
  if(is.na(l$pricecols["RollLong"]))
    l$pricecols["RollLong"] <- l$pricecols["Long"]
  ## Process expression args
  l$el <- substitute(el)
  l$es <- substitute(es)
  l$xl <- substitute(xl)
  l$xs <- substitute(xs)
  l$delta <- substitute(delta)
  l$size.at <- substitute(size.at)
  l$roll.at <- substitute(roll.at)
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
