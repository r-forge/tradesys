signals <- function(x){
  if(!is.tsts(x))
    return(c(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE))
  do.call("cbind", lapply(attr(x, "tstsp")$signals, eval, envir=as.list(as.data.frame(as.matrix(x)))))
}

exprcols <- function(x){
  if(!is.tsts(x))
    return(NULL)
  if(is.null(attr(x, "tstsp")$exprcols))
    return(NULL) 
  do.call("cbind", lapply(attr(x, "tstsp")$exprcols, eval, envir=as.list(as.data.frame(as.matrix(x)))))
}

delta <- function(x){
  if(!is.tsts(x))
    return(FALSE)
  y <- eval(attr(x, "tsts")$delta, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  if(length(y) == 1)
    return(y)
  cbind(y, states(x))[, 1] ## expand if necessary
}

size.at <- function(x){
  if(!is.tsts(x))
    return(FALSE)
  y <- eval(attr(x, "tsts")$size.at, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  as.logical(cbind(y, states(x))[, 1]) ## expand if necessary
}

roll.at <- function(x){
  if(!is.tsts(x))
    return(FALSE)
  y <- eval(attr(x, "tsts")$roll.at, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  as.logical(cbind(y, states(x))[, 1]) ## expand if necessary
}

