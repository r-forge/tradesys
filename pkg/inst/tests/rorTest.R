test.ror <- function(){

  ## Intuitive examples: Price -.5, +.5
  checkEquals(ror(c(1.0,0.5,1.0), c( 1, 1, 1), size.at=FALSE, delta=1.0), c(1.0000, 0.5000, 1.0000))
  checkEquals(ror(c(1.0,0.5,1.0), c( 1, 1, 1), size.at=TRUE,  delta=1.0), c(1.0000, 0.5000, 0.7500))
  checkEquals(ror(c(1.0,0.5,1.0), c( 1, 1, 1), size.at=TRUE,  delta=0.5), c(1.0000, 0.7500, 0.9375))
  checkEquals(ror(c(1.0,0.5,1.0), c( 1, 1, 1), size.at=TRUE,  delta=2.0), c(1.0000, 0.0000, 0.0000))
  checkEquals(ror(c(1.0,0.5,1.0), c( 1, 1, 1), size.at=TRUE,  delta=1.5), c(1.0000, 0.2500, 0.4375))
  checkEquals(ror(c(1.0,0.5,1.0), c( 1,-1, 1), size.at=FALSE, delta=1.0), c(1.0000, 0.5000, 0.0000))
  checkEquals(ror(c(1.0,0.5,1.0), c( 1,-1, 1), size.at=TRUE,  delta=1.0), c(1.0000, 0.5000, 0.2500))
  checkEquals(ror(c(1.0,0.5,1.0), c(-1, 1, 1), size.at=FALSE, delta=1.0), c(1.0000, 1.5000, 2.0000))
  checkEquals(ror(c(1.0,0.5,1.0), c(-1, 1, 1), size.at=TRUE,  delta=0.5), c(1.0000, 1.2500, 1.5625))

  ## Variable deltas
  checkEquals(ror(c(1.0,0.5,1.0), c( 1,-1, 1), size.at=TRUE, delta=c(1,1,1)), c(1.00,0.50,0.25))
  checkEquals(ror(c(1.0,0.5,1.0), c( 1,-1, 1), size.at=TRUE, delta=c(1,2,1)), c(1.00,0.50,0.0))
  checkEquals(ror(c(1.0,0.5,1.0), c( 1,-1, 1), size.at=TRUE, delta=c(1,.5,1)), c(1.00,0.50,.375))

  ## 1000 Random Prices, States and Deltas
  Prices <- rnorm(1000, 1, 1)
  States <- statemap(c(0,1,2,3,4,8)[1 + round(5 * runif(1000, 0, 1))])
  Deltas <- rnorm(1000, 1, 1)

  ## When size.at=FALSE ror = cumsum(pnl * delta) + 1
  checkEquals(ror(Prices, States, size.at=FALSE, delta=.1), cumsum(pnl(Prices, States) * .1) + 1)
  checkEquals(ror(Prices, States, size.at=FALSE, delta=2), cumsum(pnl(Prices, States) * 2) + 1)
  checkEquals(ror(Prices, States, size.at=FALSE, delta=Deltas), cumsum(pnl(Prices, States) * Deltas[1]) + 1)

  ## When size.at=TRUE ror = cumprod(pnl + 1) * delta
  checkEquals(ror(Prices, States, delta=.1, size.at=TRUE), cumprod(pnl(Prices, States) * .1 + 1))
  checkEquals(ror(Prices, States, delta=2, size.at=TRUE), cumprod(pnl(Prices, States) * 2 + 1))
  checkEquals(ror(Prices, States, delta=1, size.at=TRUE), cumprod(pnl(Prices, States) * 1 + 1))
  checkEquals(ror(Prices, States, delta=Deltas, size.at=TRUE), cumprod(pnl(Prices, States) * c(1,Deltas[-1000]) + 1))

  ##
  ## Exceptions
  ##

  ## States must be 1, 0, and -1
  checkException(ror(c(1.0,1,1.0), c(-1, 2, 1)), silent=TRUE)

  ## All arguments must be a multiple of prices (or nrow(prices))
  Prices <- 1:3
  checkException(ror(Prices, c(-1, 1),    c(FALSE,FALSE,FALSE), c(FALSE,FALSE,FALSE), c(1,1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE),       c(FALSE,FALSE,FALSE), c(1,1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE,FALSE), c(FALSE,FALSE,FALSE), c(1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE,FALSE), c(FALSE,FALSE),       c(1,1,1)))

  Prices <- matrix(1:6, ncol=2)
  checkException(ror(Prices, c(-1, 1),    c(FALSE,FALSE,FALSE), c(FALSE,FALSE,FALSE), c(1,1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE),       c(FALSE,FALSE,FALSE), c(1,1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE,FALSE), c(FALSE,FALSE,FALSE), c(1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE,FALSE), c(FALSE,FALSE),       c(1,1,1)))

  Prices <- matrix(1:9, ncol=3)
  checkException(ror(Prices, c(-1, 1),    c(FALSE,FALSE,FALSE), c(FALSE,FALSE,FALSE), c(1,1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE),       c(FALSE,FALSE,FALSE), c(1,1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE,FALSE), c(FALSE,FALSE,FALSE), c(1,1)))
  checkException(ror(Prices, c(-1, 1, 1), c(FALSE,FALSE,FALSE), c(FALSE,FALSE),       c(1,1,1)))
  
  ##
  ## Tests of Deprecated Function 'equity'
  ##
  
  ## E(t+1) = E(t) when t is a trade exit (BUG FIX at Revision 93)
  checkEquals(equity(c(100,113,80,70,10), c(1,0,0,0,0), delta=1, size.at=FALSE)[, "Equity"], c(1,1.13,1.13,1.13,1.13))

  ## Test equity vs ror
  Prices <- rnorm(1000, 1, 1)
  checkEquals(ror(Prices, 1, delta=2, size.at=FALSE), equity(Prices, 1, delta=2, size.at=FALSE, percent=FALSE)[, "Equity"])
  checkEquals(ror(Prices, 1, delta=2, size.at=TRUE), equity(Prices, 1, delta=2, size.at=TRUE, percent=FALSE)[, "Equity"])

}
