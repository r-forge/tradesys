test.ror <- function(){

  ## Intuitive examples
  checkEquals(ror(c(100,50,100), c( 1,-1, 1), delta=.01,  size.at=FALSE), c(1.0000, 0.5000, 0.0000))
  checkEquals(ror(c(100,50,100), c( 1, 1, 1), delta=.01,  size.at=FALSE), c(1.0000, 0.5000, 1.0000))
  checkEquals(ror(c(100,50,100), c( 1, 1, 1), delta=.01,  size.at=TRUE),  c(1.0000, 0.5000, 0.7500))
  checkEquals(ror(c(100,50,100), c(-1, 1, 1), delta=.01,  size.at=FALSE), c(1.0000, 1.5000, 2.0000))
  checkEquals(ror(c(100,50,100), c( 1, 1, 1), delta=.005, size.at=TRUE),  c(1.0000, 0.7500, 0.9375))
  checkEquals(ror(c(100,50,100), c( 1, 1, 1), delta=.005, size.at=FALSE), c(1.0000, 0.7500, 1.0000))
  checkEquals(ror(c(100,50,100), c( 1, 1, 1), delta=.015, size.at=TRUE),  c(1.0000, 0.2500, 0.4375))
  checkEquals(ror(c(100,50,100), c( 1, 1, 1), delta=.015, size.at=FALSE), c(1.0000, 0.2500, 1.0000))

  prc <- c(1.00, 1.10, 1.20, 1.00, 0.75, 0.90, 1.00)
  
  ## When size.at=FALSE ror is cumsum(pnl) + 1
  checkEquals(ror(prc, 1, delta=2, size.at=FALSE), cumprod(ror(prc, 1, delta=2, size.at=FALSE, period=TRUE) + 1))
  checkEquals(ror(prc, 1, delta=2, size.at=FALSE), cumsum(pnl(prc, 1) * 2) + 1)

  ## When size.at=TRUE ror is cumprod(pnl + 1)
  checkEquals(ror(prc, 1, delta=2, size.at=TRUE), cumprod(pnl(prc, 1) * 2 + 1))

  Prices <- 1 + rnorm(100, sd=.01)
  States <- rep(c(rep(1, 10), rep(-1, 10)), 100 / 20) ## alternating longs / shorts length 10

  ## CRoR(t) = [1 + sum(PnL(j+1), PnL(j+2), ..., PnL(t)) * delta(j)] * CRoR(j)
  x <- ror(Prices, states=States, delta=.25, size.at=phasemap(States) != 0)
  checkEquals((1 + sum(pnl(Prices, States)[1:5]) * .25) * x[1], x[5], checkNames=FALSE)
  checkEquals((1 + sum(pnl(Prices, States)[1:10]) * .25) * x[1], x[10], checkNames=FALSE)
  checkEquals((1 + sum(pnl(Prices, States)[12:13]) * .25) * x[11], x[13], checkNames=FALSE)
  checkEquals((1 + sum(pnl(Prices, States)[22:29]) * .25) * x[21], x[29], checkNames=FALSE)

  ##
  ## Tests of Deprecated Function 'equity'
  ##
  
  ## E(t+1) = E(t) when t is a trade exit (BUG FIX at Revision 93)
  checkEquals(equity(c(100,113,80,70,10), c(1,0,0,0,0), delta=1, size.at=FALSE)[, "Equity"], c(1,1.13,1.13,1.13,1.13))

  ## Test equity vs ror
  checkEquals(ror(prc, 1, delta=2, size.at=FALSE), equity(prc, 1, delta=2, size.at=FALSE)[, "Equity"])
  checkEquals(ror(prc, 1, delta=2, size.at=TRUE), equity(prc, 1, delta=2, size.at=TRUE, percent=FALSE)[, "Equity"])

}
