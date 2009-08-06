coredata.tsts <- function(x, ...){
  y <- as.matrix.tsts(x)
  ## remove formulae (if any), Equity, and St
  y <- y[, -which(colnames(y) %in% c(names(tsys(x)$formulae), "Equity", "St"))]
  zoo(y, order.by=index(x))
}

"coredata<-.tsts" <- function(x, value){
  l <- as.list(tsys(x))
  l$data <- value
  l$order.by <- index(value)
  do.call("tsts", l)
}
