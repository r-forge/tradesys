coredata.tsts <- function(x, ...){
  y <- as.matrix.tsts(x)
  if(!is.null(attr(x, "tstsp")$exprcols)) ## remove exprcols, if any
    y <- y[, -match(names(attr(x, "tstsp")$exprcols), colnames(y))]
  y <- y[, -which(colnames(y) %in% c("Equity","St"))]
  zoo(y, order.by=index(x))
}

"coredata<-.tsts" <- function(x, value){
  as.tsts(value)
}
