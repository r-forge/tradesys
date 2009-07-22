states <- function(x){
  if(!"St" %in% colnames(x))
    stop("x doesn't contain a states column 'St'.")
  y <- x[, "St"]
  names(y) <- NULL
  y
}

"states<-" <- function(x, value){
  if(!"St" %in% colnames(x))
    stop("x doesn't contain a states column 'St'.")
  if(!any(value %in% c(1,0,-1)))
    stop("value must be a numeric vector containing only 1, 0, and -1 values.")
  if(nrow(x) %% length(value) != 0)
    stop("length(value) must be a multiple of nrow(x).")
  x[, "St"] <- cbind(x[, "St"], value)[, 2]
  x
}
