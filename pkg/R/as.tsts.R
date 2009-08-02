as.tsts <- function(x, ...){
  UseMethod("as.tsts", x)
}

as.tsts.default <- function(x, ...){
  if(is.tsts(x))
    ARGS <- tstsp(x)
  else
    ARGS <- list(...)
  ARGS <- c(list(data=coredata(x)), list(order.by=index(x)), ARGS)
  do.call("tsts", ARGS)
}

