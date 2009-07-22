tsts <- function(data, order.by=index(data), states=0, roll.at=NULL, pricecols=1){
  Index <- order.by
  CoreAttr <- attributes(data)
  data <- as.matrix(data)
  if(length(colnames(data)) != unique(length(colnames(data))))
    stop("data must have unique column names.")
  if(nrow(data) %% length(states) != 0)
    stop("length(states) must be a multiple of nrow(data)")
  if(any(!states %in% c(0,1,-1)))
    stop("states must be a vector composed of 0, 1, and -1.")
  data <- cbind(data, St=states)
  if(length(Index) != length(unique(Index)))
    stop("all order.by must be unique.")
  if(length(Index) != nrow(data))
    stop("length(order.by) must equal nrow(data)")
  ## valuation column cannot have NA's. other price cols with NA's get assigned valuation price
  if(any(is.na(data[, pricecols$valuation])))
    stop(paste("NA's are not allowed in the valuation price column", pricecols$valuation))
  for(col in pricecols){
    if(any(n <- is.na(data[, col])))
      data[which(n), col] <- data[which(n), pricecols$valuation]
  }
  attr(data, "index") <- Index
  attr(data, "tsts") <- list(roll.at=NULL, pricecols=pricecols, coreattr=CoreAttr,
                             exprcols=NULL, exprsigs=list(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE),
                             entrywins=FALSE, entrycond=FALSE)
  class(data) <- "tsts"
  if(!is.null(roll.at))
    roll.at(data) <- roll.at
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
