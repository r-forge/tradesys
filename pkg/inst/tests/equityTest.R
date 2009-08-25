test.equity <- function(){

  ## intuitive examples
  checkEquals(equity(c(100,50,100), c(1,-1,1), delta=1, size.at=FALSE)[, "Equity"], c(1,.5,0))
  checkEquals(equity(c(100,50,100), c(1,1,1), delta=1, size.at=FALSE)[, "Equity"], c(1,.5,1))
  checkEquals(equity(c(100,50,100), c(-1,1,1), delta=1, size.at=FALSE)[, "Equity"], c(1,1.5,3))
  checkEquals(equity(c(100,50,100), c(1,1,1), delta=1, size.at=FALSE)[, "Equity"], c(1,.5,1))
  checkEquals(equity(c(100,50,100), c(1,1,1), delta=1, size.at=TRUE)[, "Equity"], c(1,.5,1))
  checkEquals(equity(c(100,50,100), c(1,1,1), delta=.5, size.at=TRUE)[, "Equity"], c(1,.75,1.125))
  checkEquals(equity(c(100,50,100), c(1,1,1), delta=.5, size.at=FALSE)[, "Equity"], c(1,.75,1))
  checkEquals(equity(c(100,50,100), c(1,1,1), delta=1.5, size.at=TRUE)[, "Equity"], c(1,.25,.625))
  checkEquals(equity(c(100,50,100), c(1,1,1), delta=1.5, size.at=FALSE)[, "Equity"], c(1,.25,1))
  checkEquals(equity(c(100,50,100,110,220,110), c(1,1,1,1,1,1), delta=1, size.at=TRUE)[, "HPR"], c(0,-.5,1,.1,1,-.5))
  checkEquals(equity(c(100,50,100,110,220,110), c(1,1,1,1,1,1), delta=1, size.at=FALSE)[, "HPR"], c(0,-.5,0,.1,1.2,.1))  

  set.seed(8091)
  Price <- 1 + rnorm(100, sd=.01)

  ## all states 1 so equity is simple function of price
  checkEquals((Price[100] / Price[1] - 1) * 1 + 1,
              equity(Price, states=1, delta=1)[100, "Equity"],
              checkNames=FALSE)

  checkEquals((Price[100] / Price[1] - 1) * 2 + 1,
              equity(Price, states=1, delta=2)[100, "Equity"],
              checkNames=FALSE)

  checkEquals((Price[100] / Price[1] - 1) * .25 + 1,
              equity(Price, states=1, delta=.25)[100, "Equity"],
              checkNames=FALSE)

  checkEquals(unique(equity(Price, states=1, delta=0)[, "Equity"]), 1, checkNames=FALSE)

  States <- rep(c(rep(1, 10), rep(-1, 10)), 100/20) ## alternating longs / shorts length 10

  ## percent=TRUE so E(t) = [p(t) / p(i) - 1] * s(t-1) * delta * E(i) + E(i)
  x <- equity(Price, states=States, delta=.25)
  checkEquals((Price[5] / Price[1] - 1)   *  .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[5, "Equity"], checkNames=FALSE)
  checkEquals((Price[10] / Price[1] - 1)  *  .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[10, "Equity"], checkNames=FALSE)
  checkEquals((Price[17] / Price[11] - 1) * -.25 * x[11, "Equity"] + x[11, "Equity"], x[17, "Equity"], checkNames=FALSE)
  checkEquals((Price[20] / Price[11] - 1) * -.25 * x[11, "Equity"] + x[11, "Equity"], x[20, "Equity"], checkNames=FALSE)

  ## percent=FALSE so E(t) = [p(t) - p(i)] * s(i-1) * delta * E(i) + E(i)
  x <- equity(Price, states=States, delta=.25, percent=FALSE)
  checkEquals((Price[5] -  Price[1])  * .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[ 5, "Equity"], checkNames=FALSE)
  checkEquals((Price[10] - Price[1])  * .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[10, "Equity"], checkNames=FALSE)
  checkEquals((Price[12] - Price[11]) *-.25 * x[11, "Equity"] + x[11, "Equity"], x[12, "Equity"], checkNames=FALSE)
  checkEquals((Price[29] - Price[21]) * .25 * x[21, "Equity"] + x[21, "Equity"], x[29, "Equity"], checkNames=FALSE)

  ## roll.at examples
  checkEquals(equity(cbind(c(100,100,50,60), c(55,50,NA,NA)), c(1,1,1,1), roll.at=c(FALSE,TRUE,FALSE,FALSE), delta=1)[, "Equity"], c(1,1,1,1.2))
  checkEquals(equity(cbind(c(100,100,50,12), c(55,50,10,NA)), c(1,1,1,1), roll.at=c(FALSE,TRUE,TRUE,FALSE), delta=1)[, "Equity"], c(1,1,1,1.2))

}

