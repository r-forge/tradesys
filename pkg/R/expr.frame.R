expr.frame <- function(x, exprlist){
  y <- inside(as.data.frame(x), exprlist)
  attr(y, "exprlist") <- exprlist
  structure(y, class="expr.frame")
}

exprlist <- function(x){
  attr(x, "exprlist")
}

"$<-.expr.frame" <- function(x, name, ..., value){
  if(is.call(value)){ ## if value is an expression..
    l <- exprlist(x)
    l[[name]] <- value
    y <- inside(coredata(x), l)
    if(name %in% names(exprlist(x))){
      y <- y[, match(names(y), names(x))]
    }else{
      y <- y[, match(names(y), c(names(x), name))]
    }
    attr(y, "exprlist") <- l
    return(structure(y, class = "expr.frame"))
  }
  ## Make the assignment to coredata(x) and re-create the expr.list
  d <- coredata(x)
  d[[name]] <- value
  d <- inside(d, exprlist(x)[setdiff(names(exprlist(x)), name)])
  d <- d[, match(names(d), unique(c(names(x), name)))]
  attr(d, "exprlist") <- exprlist(x)
  structure(d, class="expr.frame")
}

## Column-subsetting [, [<- Methods
## Row-subsetting
## rbind
## cbind / merge

as.data.frame.expr.frame <- function(x, ...){
  attributes(x)$exprlist <- NULL
  structure(x, class="data.frame")
}

as.list.expr.frame <- function(x, ...){
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


