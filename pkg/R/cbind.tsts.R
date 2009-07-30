cbind.tsts <- function(..., deparse.level=1){
  x <- list(...)[[1]] ## tsts object here
  l <- list(...)[-1]  ## vectors, matrices or expressions here
  y <- as.matrix(x)
  for(i in 1:length(l)){
    if(is.expression(l[[i]])){
      attr(x, "tsts")$exprcols[[names(l)[i]]] <- l[[i]]
      l[[i]] <- eval(l[[i]], as.list(as.data.frame(as.matrix(x))))
    }else{
      l[[i]] <- cbind(l[[i]], x[, 1])[, 1] ## expand if necessary
    }
    if(names(l)[i] %in% colnames(x)){
      y[, names(l)[i]] <- cbind(l[[i]], y[, 1])[, 1]
      l <- l[-i]
    }
  }
  y <- cbind(y, do.call("cbind", l))
  attr(y, "index") <- attr(x, "index") 
  attr(y, "tsts") <- attr(x, "tsts")
  class(y) <- "tsts"
  y
}
