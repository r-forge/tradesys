coredata.tsts <- function(x, ...){
  y <- as.matrix.tsts(x)
  ## remove exprcols (if any), Equity, and St
  y <- y[, -which(colnames(y) %in% c(names(attr(x, "tstsp")$exprcols), "Equity", "St"))]
  zoo(y, order.by=index(x))
}

"coredata<-.tsts" <- function(x, value){
  as.tsts(value)
}
