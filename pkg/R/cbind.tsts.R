cbind.tsts <- function(..., deparse.level=1){
  x <- list(...)[[1]] ## tsts object here
  l <- list(...)[-1]  ## vectors, matrices or expressions here
  y <- coredata(x)
  y <- cbind(y, do.call("cbind", l))
  tsts(y, index(x), tsys=tsys(x))
}
