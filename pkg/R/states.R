states <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'")
  y <- x[, "states"]
  names(y) <- NULL
  y
}
