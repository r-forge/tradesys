as.tsts <- function(x, ...){
  UseMethod("as.tsts")
}

as.tsts.default <- function(x, order.by=index(x), states=NULL, roll.at=NULL, pricecols=1, ...){
  tsts(x, order.by, states, roll.at, pricecols)
}
