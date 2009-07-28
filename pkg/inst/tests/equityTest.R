test.randomprice <- function(){

  randPrice <- function(number){
    1 + rnorm(number, sd=.01)
  }
  randStates <- function(number){
    round(abs(2) * runif(number, 0, 1)) -1
  }
  altnStates <- function(number){ ## alternating longs / shorts length 10
    rep(c(rep(1, 10), rep(-1, 10)), number/20)
  }

  ## all states 1 so equity is simple function of price
  x <- equity(prices <- randPrice(100), states=1, delta=1)
  checkEquals((prices[100] / prices[1] - 1) * 1 + 1, x[100, "Equity"], checkNames=FALSE)
  
  x <- equity(prices <- randPrice(100), states=1, delta=2)
  checkEquals((prices[100] / prices[1] - 1) * 2 + 1, x[100, "Equity"], checkNames=FALSE)

  x <- equity(prices <- randPrice(100), states=1, delta=.25)
  checkEquals((prices[100] / prices[1] - 1) * .25 + 1, x[100, "Equity"], checkNames=FALSE)
  
  x <- equity(prices <- randPrice(100), states=1, delta=0)
  checkEquals(unique(x[, "Equity"]), 1, checkNames=FALSE)

  ## percent=TRUE so E(t) = [p(t) / p(i) - 1] * s(t-1) * delta * E(i) + E(i)
  x <- equity(prices <- randPrice(100), states=altnStates(100), delta=.25)
  checkEquals((prices[5] / prices[1] - 1)   *  .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[5, "Equity"], checkNames=FALSE)
  checkEquals((prices[10] / prices[1] - 1)  *  .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[10, "Equity"], checkNames=FALSE)
  checkEquals((prices[17] / prices[11] - 1) * -.25 * x[11, "Equity"] + x[11, "Equity"], x[17, "Equity"], checkNames=FALSE)
  checkEquals((prices[20] / prices[11] - 1) * -.25 * x[11, "Equity"] + x[11, "Equity"], x[20, "Equity"], checkNames=FALSE)

  ## percent=FALSE so E(t) = [p(t) - p(i)] * s(i-1) * delta * E(i) + E(i)
  x <- equity(prices <- randPrice(100), states=altnStates(100), delta=.25, percent=FALSE)
  checkEquals((prices[5] -  prices[1])  * .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[ 5, "Equity"], checkNames=FALSE)
  checkEquals((prices[10] - prices[1])  * .25 * x[ 1, "Equity"] + x[ 1, "Equity"], x[10, "Equity"], checkNames=FALSE)
  checkEquals((prices[12] - prices[11]) *-.25 * x[11, "Equity"] + x[11, "Equity"], x[12, "Equity"], checkNames=FALSE)
  checkEquals((prices[29] - prices[21]) * .25 * x[21, "Equity"] + x[21, "Equity"], x[29, "Equity"], checkNames=FALSE)

  ## resize=TRUE so E(t) = ([p(t) / p(t-1) - 1] * s(t-1) * delta + 1) * E(t-1)
  x <- equity(prices <- randPrice(100), states=altnStates(100), delta=.25, size.at=TRUE)
  checkEquals(((prices[5] / prices[4] - 1)   *  .25 + 1) * x[ 4, "Equity"], x[5, "Equity"], checkNames=FALSE)
  checkEquals(((prices[7] / prices[6] - 1)   *  .25 + 1) * x[ 6, "Equity"], x[7, "Equity"], checkNames=FALSE)
  checkEquals(((prices[11] / prices[10] - 1) *  .25 + 1) * x[10, "Equity"], x[11, "Equity"], checkNames=FALSE)
  checkEquals(((prices[12] / prices[11] - 1) * -.25 + 1) * x[11, "Equity"], x[12, "Equity"], checkNames=FALSE)
}

test.randomprice()
