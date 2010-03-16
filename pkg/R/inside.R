inside <- function(data, exprlist, ...){
  if(!is.list(data)){
    stop("data must be a list or a data.frame.")
  }
  if(is.null(names(exprlist))|
     any(duplicated(names(exprlist)))|
     "" %in% names(exprlist)|
     any(names(exprlist) %in% names(data))){
    stop("names(exprlist) must be non-null and unique.")
  }
  f <- function(){
    lapply(as.list(names(formals())), function(x) eval(as.name(x)))
  }
  formals(f) <- c(as.list(as.data.frame(data)), exprlist)
  x <- f()
  if(is.data.frame(data)){
    x <- structure(as.data.frame(x), row.names=rownames(data))
  }
  structure(x, names=names(formals(f)))
}




