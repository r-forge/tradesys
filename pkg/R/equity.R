equity <- function(prices, states, delta=1, size.at=as.logical(c(states[1], diff(states))), roll.at=FALSE, percent=TRUE){
  ## process prices
  prices <- as.matrix(prices)
  if(ncol(prices) == 1)
    prices <- cbind(prices, prices) 
  rrices <- as.vector(prices[, 2])
  prices <- as.vector(prices[, 1])
  ## states, delta, size.at and roll.at length must be multiple of price
  states <- cbind(states, prices)[, 1]
  delta <- cbind(delta, prices)[, 1]
  roll.at <- as.logical(cbind(roll.at, prices)[, 1])
  size.at <- as.logical(cbind(size.at, prices)[, 1])
  pricesLag <- prices
  pricesLag[which(!size.at & !roll.at)] <- NA
  pricesLag[1] <- prices[1]
  pricesLag <- pricesLag + (rrices - prices) * as.numeric(roll.at)
  pricesLag <- na.locf(pricesLag, na.rm=FALSE)
  PnL <- 0
  RoR <- 0
  if(length(prices) > 1){
    PnL <- c(PnL, (prices[2:length(prices)] - pricesLag[-length(pricesLag)]) * states[-length(states)])
    RoR <- c(RoR, (prices[2:length(prices)] / pricesLag[-length(pricesLag)] - 1) * states[-length(states)])
  }
  EquityLag <- rep(1, length(prices))
  if(percent)
    Equity <- 1 + RoR * delta
  else
    Equity <- 1 + PnL * delta
  if(length(prices) > 1){ ## recursively solve Equity adjusting for resizing
    sapply(2:length(prices), function(i){
      e <- Equity
      elag <- EquityLag
      e[i] <- Equity[i] * EquityLag[i-1]
      if(size.at[i])
        elag[i] <- e[i]
      else
        elag[i] <- elag[i-1]
      assign("Equity", e, inherits=TRUE)
      assign("EquityLag", elag, inherits=TRUE)
    })
  }
  cbind(States=states, Trade=cumsum(as.logical(c(states[1], diff(states)))),
        Size=size.at, Roll=roll.at, Delta=delta, Prices=prices, PnL, RoR, Equity)
}
