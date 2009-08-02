as.tsts <- function(x, ...){
  UseMethod("as.tsts", x)
}

as.tsts.default <- function(x, ...){
  if(is.tsts(x))
    ARGS <- tstsp(x)
  else
    ARGS <- list(...)
  print(ARGS)
  do.call("tsts", ARGS)
}

