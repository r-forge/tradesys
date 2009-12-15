coredata.tsts <- function(x, ...){
  y <- as.matrix.tsts(x)
  ExclCols <- which(colnames(y) %in% names(tsys(x)))
  if(length(ExclCols) > 1)
    y <- y[, -ExclCols]
  rownames(y) <- NULL
  zoo(y, order.by=index(x))
}

"coredata<-.tsts" <- function(x, value){
  tsts(value, index(value), tsys=tsys(x))
}
