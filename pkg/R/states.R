states <- function(x){
  if(!"St" %in% colnames(x))
    return(FALSE)
  y <- x[, "St"]
  names(y) <- NULL
  y
}

## RE-WRITE
## "states<-" <- function(x, value){
##   tsys(x)$states <- value
##   x
## }
