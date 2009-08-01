signals <- function(x, list=NULL){
  if(is.null(list))
    list <- attr(x, "tstsp")$signals
  do.call("cbind", lapply(list, eval, envir=c(index=index(x), as.list(as.data.frame(as.matrix(x))))))
}

"signals<-" <- function(x, type="el", value){
  type <- tolower(type[1])
  if(!type %in% c("el","es","xl","xs"))
    stop("type must be one of 'el', 'es', 'xl', 'xs'.")
  attr(x, "tstsp")$signals[[type]] <- value
  x
}

exprcols <- function(x, list=NULL){
  if(is.null(list))
    list <- attr(x, "tstsp")$exprcols
  do.call("cbind", lapply(list, eval, envir=c(index=index(x), as.list(as.data.frame(as.matrix(x))))))
}

delta <- function(x, expr){
  if(missing(expr))
    expr <- attr(x, "tstsp")$delta
  y <- eval(expr, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  if(length(y) == 1)
    return(y)
  cbind(y, states(x))[, 1] ## expand if necessary
}

size.at <- function(x, expr){
  if(missing(expr))
    expr <- attr(x, "tstsp")$size.at
  y <- eval(expr, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  as.logical(cbind(y, states(x))[, 1]) ## expand if necessary
}

roll.at <- function(x, expr){
  if(missing(expr))
    expr <- attr(x, "tstsp")$roll.at
  y <- eval(expr, c(list(index=index(x)), as.list(as.data.frame(as.matrix(x)))))
  as.logical(cbind(y, states(x))[, 1]) ## expand if necessary
}

