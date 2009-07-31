states <- function(x){
  if(!"St" %in% colnames(x))
    return(FALSE)
  y <- x[, "St"]
  names(y) <- NULL
  y
}

"states<-" <- function(x, value){
  y <- as.matrix(x)
  if(!is.matrix(y))
    stop("x must be a matrix")
  if(nrow(y) %% length(value) != 0)
    stop("length(value) must be a multiple of nrow(x)")
  if(!any(value %in% c(1,0,-1)))
    stop("value must be a numeric vector containing only 1, 0, and -1 values")
  y[, "St"] <- cbind(value, y[, 1])[, 1] ## expand if necessary
  attributes(y) <- attributes(x)
  y
}
