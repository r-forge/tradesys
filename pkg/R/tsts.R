tsts <- function(data, order.by=index(data), ..., tsys=NULL, tsysvars=c("States","Equity")){
  if(is.null(tsys)){
    l <- tradesys(datavars=colnames(data), ...)
  }else{
    tsys$datavars <- colnames(data)
    l <- tsys
  }
  x <- tradesys.frame(l, data, order.by)[, tsysvars]
  if(is.null(tsysvars))
    tsysvars <- colnames(x)
  else
    tsysvars <- unique(tsysvars)
  if(any(!tsysvars %in% colnames(x)))
    stop("all tsysvars must be a column in the return of tradesys.frame.")
  x <- cbind(as.matrix(x), coredata(data))
  attr(x, "index") <- order.by
  attr(x, "tsys") <- l
  attr(x, "tsysvars") <- tsysvars
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
  tsts(coredata(x), index(x), tsys=value, tsysvars=tsysvars(x))
}

tsysvars <- function(x){
  attr(x, "tsysvars")
}

"tsysvars<-" <- function(x, value){
  tsts(coredata(x), index(x), tsys=tsys(x), tsysvars=value)
}
