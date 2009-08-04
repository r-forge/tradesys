cbind.tsts <- function(..., deparse.level=1){
  x <- list(...)[[1]] ## tsts object here
  l <- list(...)[-1]  ## vectors, matrices or expressions here
  y <- coredata(x)
  exprcols <- list()
  for(i in 1:length(l)){
    if(is.call(l[[i]])){
      exprcols[[names(l)[i]]] <- l[[i]]
    }else{
      y <- cbind(y, l[[i]])
      colnames(y)[ncol(y)] <- names(l)[i]
    }
  }
  k <- tstsp(x)
  if(length(exprcols) > 0)
    k$exprcols <- c(k$exprcols, exprcols)
  do.call("tsts", c(list(data=y), list(order.by=index(x)), k))
}
