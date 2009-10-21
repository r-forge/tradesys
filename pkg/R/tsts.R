tsts <- function(data, order.by=index(data), ..., tsys=NULL){
  if(is.null(tsys))
    l <- tradesys(...)
  else
    l <- tsys
  x <- tradesys.frame(l, data, order.by)
  x <- x[, -which(colnames(x) == "Index")]
  x <- as.matrix(x)
  attr(x, "index") <- order.by
  attr(x, "tsys") <- l
  class(x) <- c("tsts","zoo")
  x
}

###
### METHODS CLASS 'tsts'
###

head.tsts <- function(x, ...){
  head(as.zoo.tsts(x))
}

tail.tsts <- function(x, ...){
  tail(as.zoo.tsts(x))
}

print.tsts <- function(x, ...){
  print(as.zoo.tsts(x))
}

as.matrix.tsts <- function(x, ...){
  rownames(x) <- format(attr(x, "index"))
  attr(x, "index") <- NULL
  attr(x, "tsys") <- NULL
  class(x) <- "matrix"
  x
}

as.zoo.tsts <- function(x, ...){
  zoo(as.matrix.tsts(x), order.by=attr(x, "index"))
}

"[.tsts" <- function(x, i, j, drop = FALSE, ...){
  as.zoo.tsts(x)[i, j, drop = drop, ...]
}

##
## Extractors/Assignors
##

tsys <- function(x){
  if(!is.tsts(x))
    return(invisible(NULL))
  attr(x, "tsys")
}

"tsys<-" <- function(x, value){
  tsts(coredata(x), index(x), tsys=value)
}


