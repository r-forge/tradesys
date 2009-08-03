signals <- function(x){
  if(!is.tsts(x))
    return(invisible(NULL))
  l <- list(el=attr(x, "tstsp")$el, es=attr(x, "tstsp")$es, xl=attr(x, "tstsp")$xl, xs=attr(x, "tstsp")$xs)
  zoo(do.call("cbind", lapply(l, eval, envir=c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))),
      order.by=index(x))
}

exprcols <- function(x){
  if(!is.tsts(x))
    return(invisible(NULL))
  do.call("cbind", lapply(attr(x, "tstsp")$exprcols, eval, envir=c(index=index(x), as.list(as.data.frame(as.matrix(x))))))
}

delta <- function(x){
  if(!is.tsts(x))
    return(invisible(NULL))
  y <- eval(attr(x, "tstsp")$delta, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  if(length(y) == 1)
    return(y)
  cbind(y, states(x))[, 1] ## expand if necessary
}

size.at <- function(x){
  if(!is.tsts(x))
    return(invisible(NULL))
  y <- eval(attr(x, "tstsp")$size.at, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  as.logical(cbind(y, states(x))[, 1]) ## expand if necessary
}

roll.at <- function(x){
  if(!is.tsts(x))
    return(invisible(NULL))
  y <- eval(attr(x, "tstsp")$roll.at, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  as.logical(cbind(y, states(x))[, 1]) ## expand if necessary
}

