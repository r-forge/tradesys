coredata.tsts <- function(x, ...){
  y <- x[, 1:attr(x, "tsts")$coreattr$dim[2]]
  attributes(y) <- attr(x, "tsts")$coreattr
  y
}

"coredata<-.tsts" <- function(x, value){
  attr(x, "tsts")$coreattr <- attributes(value)
  ## Evaluate columns
  l <- list(index=attr(x, "index"), roll.at=attr(x, "tsts")$roll.at,
            entrywins=attr(x, "tsts")$entrywins, entrycond=attr(x, "tsts")$entrycond)
  if(!is.null(attr(x, "tsts")$columns)){ 
    cols <- lapply(attr(x, "tsts")$columns, eval, c(l, as.list(as.data.frame(value))))
    if(any(unlist(lapply(cols, function(x) (nrow(value) %% length(x)) != 0)))) 
      stop("expressions in 'columns' attribute must evaluate to a vector whose length is a multiple of nrow(value)")
    value <- cbind(value, do.call("cbind", cols))    
  }
  ## Evaluate signals
  s <- signalmap(eval(attr(x, "tsts")$signals$el, c(l, as.list(as.data.frame(value)))),
                 eval(attr(x, "tsts")$signals$es, c(l, as.list(as.data.frame(value)))),
                 eval(attr(x, "tsts")$signals$xl, c(l, as.list(as.data.frame(value)))),
                 eval(attr(x, "tsts")$signals$xs, c(l, as.list(as.data.frame(value)))),
                 attr(x, "tsts")$entrycond, attr(x, "tsts")$entrywins)
  y <- cbind(value, St=s)
  attr(y, "tsts") <- attr(x, "tsts")
  class(y) <- "tsts"
  y
}
