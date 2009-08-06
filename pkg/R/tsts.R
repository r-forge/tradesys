tsts <- function(data, order.by=index(data), datacols=colnames(data), ...){
  lt <- tradesys(datacols=datacols, ...)
  lv <- solvets(lt, data, order.by)
  data <- cbind(St=lv$St, Equity=lv$Equity, coredata(data), lv$formulae)
  attr(data, "index") <- order.by
  attr(data, "tsys") <- lt  
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

## "[.tsts" <- function(x, i, j, ..., drop=TRUE){
##   y <- as.matrix(x)
##   if(missing(i))
##     i <- 1:nrow(y)
##   if(missing(j))
##     j <- 1:ncol(y)
##   if(is.numeric(j))
##     j <- colnames(x)[j]
##   ## deal with non-existent character j's
##   ## deal with non-existent numeric j's
##   KlasCols <- c(tstsp(x)$pricecols, "St", "Equity")
##   if(all(KlasCols %in% j)){
##     TSYS <- tstsp(x)
##     TSYS$order.by <- index(x)
##     as.tsts(y[, j[-match(c("St","Equity"), j)]], index(x), TSYS)
##   }
##   if(length(colnames(y)[-j]) > 0){ 
##     if(any(colnames(y)[-j] %in% c(tstsp(x)$pricecols, "St", "Equity")))
##       return(zoo(y[, j], order.by=index(x)))
##   }
  
## }

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
