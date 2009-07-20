tsts <- function(data, order.by=index(data), states=0, roll.at=NULL, pricecols=1){
  data <- as.matrix(data)
  if(length(colnames(data)) != unique(length(colnames(data))))
    stop("data must have unique column names.")
  if(nrow(data) %% length(states) != 0)
    stop("length(states) must be a multiple of nrow(data)")
  if(any(!states %in% c(0,1,-1)))
    stop("states must be a vector composed of 0, 1, and -1.")
  data <- cbind(data, states=states)
  names(order.by) <- NULL ## Huh.. whence the "" names?
  if(length(order.by) != length(unique(order.by)))
    stop("all order.by must be unique.")
  if(length(order.by) != nrow(data))
    stop("length(order.by) must equal nrow(data)")
  if(!is.null(roll.at)){
    if(any(class(roll.at) != class(order.by)))
      stop("roll.at and order.by must be the same class")
    if(any(!roll.at %in% order.by))
      stop("all roll.at must be in order.by")
  }
  ## process pricecols parameter
  if(is.list(pricecols)){
    names(pricecols) <- tolower(names(pricecols))
    l <- list(valuation=1, enterlong=1, entershort=1, exitlong=1, exitshort=1, rolllong=1, rollshort=1)
    n <- which(names(pricecols) %in% c("valuation","enterlong","entershort","exitlong","exitshort","rolllong","rollshort"))
    if(length(n) != 0)
      l <- replace(l, match(names(pricecols[n]), names(l)), pricecols[n])
    if("enter" %in% names(pricecols))
      l <- replace(l, which(names(l) %in% c("enterlong","entershort")), pricecols$enter)
    if("exit" %in% names(pricecols))
      l <- replace(l, which(names(l) %in% c("exitlong","exitshort")), pricecols$exit)
    if("long" %in% names(pricecols))
      l <- replace(l, which(names(l) %in% c("enterlong","exitlong")), pricecols$long)
    if("short" %in% names(pricecols))
      l <- replace(l, which(names(l) %in% c("entershort","exitshort")), pricecols$short)
    if("roll" %in% names(pricecols))
      l <- replace(l, which(names(l) %in% c("rolllong","rollshort")), pricecols$roll)
    lapply(l, function(x, data){
      if(x == which(colnames(data) == "states"))
        stop("pricecol maps to the 'states' column.")
      if(is.character(x))
        if(!x %in% colnames(data))
          stop(paste("pricecol", x, "is not in colnames(data)"))
      if(is.numeric(x))
        if(x > ncol(data))
          stop("a pricecol index exceeds ncol(data)")
    }, data)
    pricecols <- l
  }else{
    pricecols <- list(valuation=pricecols[1], enterlong=pricecols[1], entershort=pricecols[1], exitlong=pricecols[1],
                      exitshort=pricecols[1], rolllong=pricecols[1], rollshort=pricecols[1])
  }
  ## valuation column cannot have NA's. other price cols with NA's get assigned valuation price
  if(any(is.na(data[, pricecols$valuation])))
    stop(paste("NA's are not allowed in the valuation price column", pricecols$valuation))
  for(col in pricecols){
    if(any(n <- is.na(data[, col])))
      data[which(n), col] <- data[which(n), pricecols$valuation]
  }
  attr(data, "index") <- order.by
  attr(data, "tsts") <- list(roll.at=roll.at, pricecols=pricecols)
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
    stop("index(x) and value must be the same class.")
  if(!is.null(attr(x, "tsts")$roll.at)){
    if(class(attr(x, "tsts")$roll.at)[1] != class(value)[1])
      stop("index(x) and value must be the same class.")
    if(any(!attr(x, "tsts")$roll.at %in% value))
      stop("all roll.at values must be in value.")
  }
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

window.tsts <- function(x, index = index.tsts(x), start = NULL, end = NULL, ...){
  y <- window(as.zoo.tsts(x), index, start, end, ...)
  tsts(y[, which(colnames(y) == "states")], index(y), as.vector(y[, "states"]), attr(x, "tsts")$roll.at, attr(x, "tsts")$pricecols)
}

as.matrix.tsts <- function(x, ...){
  rownames(x) <- format(attr(x, "index"))
  attr(x, "tsts") <- NULL
  attr(x, "index") <- NULL
  class(x) <- "matrix"
  x
}

as.zoo.tsts <- function(x, ...){
  zoo(as.matrix.tsts(x), order.by=attr(x, "index"))
}
