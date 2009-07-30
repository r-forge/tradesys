tsts <- function(data, order.by=index(data), states=0, pricecols=1, ...){
  if(length(order.by) != length(unique(order.by)))
    stop("all order.by must be unique.")
  if(length(order.by) != nrow(data))
    stop("length(order.by) must equal nrow(data)")
  data <- as.matrix(data)
  if(length(colnames(data)) != unique(length(colnames(data))))
    stop("data must have unique column names.")
  if(nrow(data) %% length(states) != 0)
    stop("length(states) must be a multiple of nrow(data)")
  if(!any(states %in% c(1,0,-1)))
    stop("states must be a numeric vector containing only 1, 0, and -1 values")
  data <- cbind(data, St=states)
  l <- list(NOMATCH=NA,
            pricecols=1,
            exprcols=list(),
            signals=list(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE),
            delta=expression(1),
            roll.at=expression(FALSE),
            size.at=expression(statechg(St)),
            percent=TRUE,
            entrywins=FALSE,
            entrycond=FALSE)
  l <- replace(l, match(names(list(...)), names(l), nomatch=1), list(...))
  attr(data, "tsts") <- l[-1]
  attr(data, "index") <- order.by
  pricecols(data) <- pricecols
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
  class(x) <- "matrix"
  x
}

as.zoo.tsts <- function(x, ...){
  zoo(as.matrix.tsts(x), order.by=attr(x, "index"))
}
