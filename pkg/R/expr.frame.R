expr.frame <- function(x, exprlist){
  structure(inside(as.data.frame(x), exprlist), exprlist = exprlist, class = "expr.frame")
}

##
## Basic Methods
##

print.expr.frame <- function(x, ...){
  print(structure(x, class="data.frame"))
}

summary.expr.frame <- function(object, ...){
  summary(as.data.frame(object), ...)
}

"$<-.expr.frame" <- function(x, name, ..., value){
  if(is.call(value)){ ## if value is an expression..
    l <- exprlist(x)
    l[[name]] <- value
    d <- inside(coredata(x), l)
    if(name %in% names(exprlist(x))){ ## because we may have mixed-up core and expr cols with col subsetting
      d <- d[, match(names(d), names(x))]
    }else{
      d <- d[, match(names(d), c(names(x), name))]
    }
    return(structure(d, exprlist = l, row.names = rownames(as.data.frame(x)), class = "expr.frame"))
  }
  ## Make the assignment to coredata(x) and re-create the expr.list
  d <- coredata(x)
  d[[name]] <- value
  d <- inside(d, exprlist(x)[setdiff(names(exprlist(x)), name)])
  d <- d[, match(names(d), unique(c(names(x), name)))]
  structure(d, exprlist = exprlist(x), row.names = rownames(as.data.frame(x)), class = "expr.frame")
}

## Column-subsetting [, [<- Methods
## Row-subsetting
## rbind
## cbind / merge

##
## Coercion Methods
##

as.data.frame.expr.frame <- function(x, ...){
  structure(x, exprlist = NULL, class="data.frame")
}

as.list.expr.frame <- function(x, ...){
  as.list(as.data.frame.expr.frame(x, ...))
}

as.matrix.expr.frame <- function(x, ...){
  as.matrix(as.data.frame.expr.frame(x), ...)
}

as.zoo.expr.frame <- function(x, ...){
  zoo(as.matrix.expr.frame(x), ...)
}

dimnames.expr.frame <- function(x){
  list(attr(x, "row.names"), attr(x, "names"))
}

##
## Extraction
##

exprlist <- function(x){
  attr(x, "exprlist")
}

coredata.expr.frame <- function(x, ...){
  if(is.null(attr(x, "exprlist"))){
    return(structure(x, class="data.frame"))
  }
  as.data.frame(x[which(!names(x) %in% names(attr(x, "exprlist")))]  )
}


