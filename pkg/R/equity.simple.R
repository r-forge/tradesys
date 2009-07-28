equity.simple <- function(prices, states, delta=1, percent=TRUE){
  prices <- cbind(prices, states, delta)[, 1]
  states <- cbind(prices, states, delta)[, 2]
  delta <- cbind(prices, states, delta)[, 3]
  P0 <- prices[-length(prices)]
  P1 <- prices[2:length(prices)]
  states <- states[-length(states)]
  delta <- delta[-length(delta)]
  if(percent)
    x <- c(1, (P1 / P0 - 1) * delta * states + 1)
  else
    x <- c(1, (P1 - P0) * delta * states + 1)
  cumprod(x)
}

## equity.simple(c(100, 50, 125), c(1,1,1))
## f <- function(delta) equity.simple(c(100, 50, 125), c(1,1,1), delta, TRUE)[3]
## optimize(f, c(0, 5), maximum=TRUE)

