tsts <- function(data, order.by=index(data), ..., tsys=NULL){
  if(is.null(tsys)){
    lt <- tradesys(datacols=colnames(data), ...)
  }else{
    tsys$datacols <- colnames(data)
    lt <- tsys
  }
  lv <- syseval(lt, data, order.by)
  data <- cbind(St=lv$St, Equity=lv$Equity, coredata(data), lv$formulae)
  attr(data, "index") <- order.by
  attr(data, "tsys") <- lt  
  class(data) <- "tsts"
  data
}

###
### METHODS CLASS 'tsts'
###

"[.tsts" <- function(x, i, j, ..., drop=TRUE){
  
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
  attr(x, "tsys") <- NULL
  class(x) <- "matrix"
  x
}

as.zoo.tsts <- function(x, ...){
  zoo(as.matrix.tsts(x), order.by=attr(x, "index"))
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
