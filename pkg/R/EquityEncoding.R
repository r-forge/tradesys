delta <- function(x){
  if(!is.tsts(x))
    return(FALSE)
  y <- eval(attr(x, "tsts")$delta, c(list(index=index(x)), as.list(as.data.frame(x))))
  cbind(y, states(x))[, 1] ## expand if necessary
}

"delta<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!is.expression(value))
    value <- as.expression(value)
  attr(x, "tsts")$delta <- value
  coredata(x) <- coredata(x)
  x
}

size.at <- function(x){
  if(!is.tsts(x))
    return(FALSE)
  y <- eval(attr(x, "tsts")$size.at, c(list(index=index(x)), as.list(as.data.frame(x))))
  cbind(y, states(x))[, 1] ## expand if necessary
}

"size.at<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!is.expression(value))
    value <- as.expression(value)
  attr(x, "tsts")$size.at <- value
  coredata(x) <- coredata(x)
  x
}

roll.at <- function(x){
  if(!is.tsts(x))
    return(FALSE)
  y <- eval(attr(x, "tsts")$roll.at, c(list(index=index(x)), as.list(as.data.frame(x))))
  cbind(y, states(x))[, 1] ## expand if necessary
}

"roll.at<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!is.expression(value))
    value <- as.expression(value)
  attr(x, "tsts")$roll.at <- value
  coredata(x) <- coredata(x)
  x
}

percent <- function(x){
  if(!is.tsts(x))
    return(FALSE)
  y <- eval(attr(x, "tsts")$percent, c(list(index=index(x)), as.list(as.data.frame(x))))
  cbind(y, states(x))[, 1] ## expand if necessary
}

"percent<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!is.expression(value))
    value <- as.expression(value)
  attr(x, "tsts")$percent <- value
  coredata(x) <- coredata(x)
  x
}
