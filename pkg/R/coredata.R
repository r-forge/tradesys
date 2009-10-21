coredata.tsts <- function(x, ...){
  y <- as.matrix.tsts(x)
  y <- y[, -which(colnames(y) %in% names(tsys(x)))]
  rownames(y) <- NULL
  zoo(y, order.by=index(x))
}

"coredata<-.tsts" <- function(x, value){
  tsts(value, index(value), tsys=tsys(x))
}
