## All states are 1 so equity is a simple function of prices
x <- equity(prices <- 1 + rnorm(100, sd=.01), states=1, delta=1)
checkEquals((prices[100] / prices[1] - 1) * 1 + 1, x[100, "Equity"], checkNames=FALSE)

x <- equity(prices <- 1 + rnorm(100, sd=.01), states=1, delta=2)
checkEquals((prices[100] / prices[1] - 1) * 2 + 1, x[100, "Equity"], checkNames=FALSE)

x <- equity(prices <- 1 + rnorm(100, sd=.01), states=1, delta=.25)
checkEquals((prices[100] / prices[1] - 1) * .25 + 1, x[100, "Equity"], checkNames=FALSE)
