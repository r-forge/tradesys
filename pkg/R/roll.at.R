roll.at <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  attr(x, "tsts")$roll.at
}

"roll.at<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(any(class(value) != class(index(x))))
    stop("value and index(x) must be the same class")
  if(any(!value %in% index(x)))
    stop("all value must be in index(x)")
  attr(x, "tsts")$roll.at <- value
  x
}
