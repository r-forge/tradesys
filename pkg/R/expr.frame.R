exprlist <- function(x){
  attr(x, "exprlist")
}

"exprlist<-" <- function(x, value){
  if(is.null(value)){
    return(as.data.frame.expr.frame(x))
  }
  y <- inside(coredata(x), value)
  attr(y, "exprlist") <- value  
  structure(y[, match(names(y), c(names(x), setdiff(names(value), names(exprlist(x)))))], class = "expr.frame")
}

"$<-.expr.frame" <- function(x, name, ..., value){
  ## if value is an expression, just call exprlist<-
  if(is.call(value)){ 
    exprlist(x)[[name]] <- value
    return(x)
  }
  ## if name matches a name in exprlist, remove the item in exprlist
  if(name %in% names(exprlist(x))){ 
    exprlist(x)[[name]] <- NULL
  }
  ## Make the assignment to coredata(x) and re-create the expr.list
  d <- coredata(x)
  d[[name]] <- value
  exprlist(d) <- exprlist(x)
  d[, match(names(d), unique(c(names(x), name)))]
}

## Column-subsetting [, [<- Methods
## Row-subsetting
## rbind
## cbind / merge

as.data.frame.expr.frame <- function (x, ...){
  attributes(x)$exprlist <- NULL
  structure(x, class="data.frame")
}

as.list.expr.frame <- function (x, ...){
  as.list(as.data.frame.expr.frame(x, ...))
}

print.expr.frame <- function(x, ...){
  print(structure(x, class="data.frame"))
}

coredata.expr.frame <- function(x, ...){
  if(is.null(attr(x, "exprlist"))){
    return(structure(x, class="data.frame"))
  }
  as.data.frame(x[which(!names(x) %in% names(attr(x, "exprlist")))]  )
}


