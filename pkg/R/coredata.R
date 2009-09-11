coredata.tsts <- function(x, ...){
  y <- as.matrix.tsts(x)
  FrameCols <- c("Equity", "States","EL","ES","XL","XS","Delta","Size","Roll",
                 "Price","Mark","Long","Short","RollOut","RollIn")
  y <- y[, -which(colnames(y) %in% c(names(tsys(x)$exprvars), FrameCols))]
  rownames(y) <- NULL
  zoo(y, order.by=index(x))
}

"coredata<-.tsts" <- function(x, value){
  tsts(value, index(value), tsys=tsys(x), tsysvars=tsysvars(x))
}
