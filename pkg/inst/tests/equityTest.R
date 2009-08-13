test.equity <- function(){

  set.seed(8091)
  P1 <- 1 + rnorm(100, sd=.01)
  S1 <- rep(c(rep(1, 10), rep(-1, 10)), 100/20) ## alternating longs / shorts length 10

  ## all states 1 so equity is simple function of price

  checkEquals((P1[100] / P1[1] - 1) * 1 + 1,
              equity(P1, states=1, delta=1)[100, "Equity"],
              checkNames=FALSE)

  checkEquals((P1[100] / P1[1] - 1) * 2 + 1,
              equity(P1, states=1, delta=2)[100, "Equity"],
              checkNames=FALSE)

  checkEquals((P1[100] / P1[1] - 1) * .25 + 1,
              equity(P1, states=1, delta=.25)[100, "Equity"],
              checkNames=FALSE)

  checkEquals(unique(equity(P1, states=1, delta=0)[, "Equity"]), 1, checkNames=FALSE)

  ## percent=TRUE so E(t) = [p(t) / p(i) - 1] * s(t-1) * delta * E(i) + E(i)
  x <- equity(P1, states=S1, delta=.25)
  checkEquals((P1[5] / P1[1] - 1)   *  .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[5, "Equity"], checkNames=FALSE)
  checkEquals((P1[10] / P1[1] - 1)  *  .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[10, "Equity"], checkNames=FALSE)
  checkEquals((P1[17] / P1[11] - 1) * -.25 * x[11, "Equity"] + x[11, "Equity"], x[17, "Equity"], checkNames=FALSE)
  checkEquals((P1[20] / P1[11] - 1) * -.25 * x[11, "Equity"] + x[11, "Equity"], x[20, "Equity"], checkNames=FALSE)

  ## percent=FALSE so E(t) = [p(t) - p(i)] * s(i-1) * delta * E(i) + E(i)
  x <- equity(P1, states=S1, delta=.25, percent=FALSE)
  checkEquals((P1[5] -  P1[1])  * .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[ 5, "Equity"], checkNames=FALSE)
  checkEquals((P1[10] - P1[1])  * .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[10, "Equity"], checkNames=FALSE)
  checkEquals((P1[12] - P1[11]) *-.25 * x[11, "Equity"] + x[11, "Equity"], x[12, "Equity"], checkNames=FALSE)
  checkEquals((P1[29] - P1[21]) * .25 * x[21, "Equity"] + x[21, "Equity"], x[29, "Equity"], checkNames=FALSE)

  ## resize=TRUE so E(t) = ([p(t) / p(t-1) - 1] * s(t-1) * delta + 1) * E(t-1)
  x <- equity(P1, states=S1, delta=.25, size.at=TRUE)
  checkEquals(((P1[5] / P1[4] - 1)   *  .25 + 1) * x[ 4, "Equity"], x[5, "Equity"], checkNames=FALSE)
  checkEquals(((P1[7] / P1[6] - 1)   *  .25 + 1) * x[ 6, "Equity"], x[7, "Equity"], checkNames=FALSE)
  checkEquals(((P1[11] / P1[10] - 1) *  .25 + 1) * x[10, "Equity"], x[11, "Equity"], checkNames=FALSE)
  checkEquals(((P1[12] / P1[11] - 1) * -.25 + 1) * x[11, "Equity"], x[12, "Equity"], checkNames=FALSE)
}
