signals <- function(x, list=NULL){
  if(is.null(list))
    list <- attr(x, "tstsp")$signals
  do.call("cbind", lapply(list, eval, envir=c(list(index=index(x)), as.list(as.data.frame(as.matrix(x))))))
}

exprcols <- function(x, list=NULL){
  if(is.null(list))
    list <- attr(x, "tstsp")$exprcols
  do.call("cbind", lapply(list, eval, envir=c(index=index(x), as.list(as.data.frame(as.matrix(x))))))
}

delta <- function(x, expr){
  if(missing(expr))
    expr <- attr(x, "tstsp")$delta
  else
    expr <- substitute(expr)
  y <- eval(expr, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  if(length(y) == 1)
    return(y)
  cbind(y, states(x))[, 1] ## expand if necessary
}

size.at <- function(x, expr){
  if(missing(expr))
    expr <- attr(x, "tstsp")$size.at
  else
    expr <- substitute(expr)
  y <- eval(expr, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  as.logical(cbind(y, states(x))[, 1]) ## expand if necessary
}

roll.at <- function(x, expr){
  if(missing(expr))
    expr <- attr(x, "tstsp")$roll.at
  else
    expr <- substitute(expr)
  y <- eval(expr, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  as.logical(cbind(y, states(x))[, 1]) ## expand if necessary
}

