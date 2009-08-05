as.tsts <- function(x, ...){
  UseMethod("as.tsts", x)
}

as.tsts.default <- function(x, order.by=index(x), ...){
  if(is.tsts(x))
    ARGS <- tsys(x)
  else
    ARGS <- list(...)
  ARGS$order.by <- order.by
  ARGS$data <- coredata(x)
  do.call("tsts", ARGS)
}
