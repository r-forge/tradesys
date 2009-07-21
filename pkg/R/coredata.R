coredata.tsts <- function(x, ...){
  y <- x[, 1:attr(x, "tsts")$coreattr$dim[2]]
  attributes(y) <- attr(x, "tsts")$coreattr
  y
}

"coredata<-.tsts" <- function(x, value){
  attr(x, "tsts")$coreattr <- attributes(value)
  ## Evaluate exprcols
  l <- list(index=attr(x, "index"), roll.at=attr(x, "tsts")$roll.at,
            entrywins=attr(x, "tsts")$entrywins, entrycond=attr(x, "tsts")$entrycond)
  if(!is.null(attr(x, "tsts")$exprcols)){ 
    cols <- lapply(attr(x, "tsts")$exprcols, eval, c(l, as.list(as.data.frame(value))))
    if(any(unlist(lapply(cols, function(x) (nrow(value) %% length(x)) != 0)))) 
      stop("expressions in exprcols attribute must evaluate to a vector whose length is a multiple of nrow(value)")
    value <- cbind(value, do.call("cbind", cols))    
  }
  ## Evaluate exprsigs
  s <- signals(eval(attr(x, "tsts")$exprsigs$el, c(l, as.list(as.data.frame(value)))),
               eval(attr(x, "tsts")$exprsigs$es, c(l, as.list(as.data.frame(value)))),
               eval(attr(x, "tsts")$exprsigs$xl, c(l, as.list(as.data.frame(value)))),
               eval(attr(x, "tsts")$exprsigs$xs, c(l, as.list(as.data.frame(value)))),
               attr(x, "tsts")$entrycond, attr(x, "tsts")$entrywins)
  y <- cbind(value, St=s)
  attr(y, "tsts") <- attr(x, "tsts")
  class(y) <- "tsts"
  y
}
