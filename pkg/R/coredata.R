coredata.tsts <- function(x, ...){
  y <- as.matrix.tsts(x)
  ## remove formulae (if any), Equity, and St
  y <- y[, -which(colnames(y) %in% c(names(attr(x, "tstsp")$formulae), "Equity", "St"))]
  zoo(y, order.by=index(x))
}

"coredata<-.tsts" <- function(x, value){
  as.tsts(value)
}
