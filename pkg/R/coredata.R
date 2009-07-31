coredata.tsts <- function(x, ...){
  y <- as.matrix.tsts(x)
  if(length(attr(x, "tsts")$exprcols) > 0) ## remove exprcols, if any
    y <- y[, -match(names(attr(x, "tsts")$exprcols), colnames(y))]
  y <- y[, -which(colnames(y) == "St")]
  zoo(y, order.by=index(x))
}

"coredata<-.tsts" <- function(x, value){
  attr(x, "tsts")$coreattr <- attributes(value)
  l <- list(index=attr(x, "index"), roll.at=attr(x, "tsts")$roll.at,
            entrywins=attr(x, "tsts")$entrywins, entrycond=attr(x, "tsts")$entrycond)
  ## Evaluate columns
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
