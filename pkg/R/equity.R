equity <- function(prices, states, delta=1, size.at=statechg(states), roll.at=FALSE, percent=TRUE){
  if(is.matrix(prices)){
    prices <- prices(prices, states)
    rrices <- prices[, "RollLong"] ## !!FIX ME!!
    prices <- prices[, "Use"]
  }else{
    rrices <- prices
  }
  ## states, delta, size.at and roll.at must be multiples of price
  states <- cbind(prices, states)[, 2]
  delta <- cbind(prices, delta)[, 2]
  roll.at <- cbind(prices, roll.at)[, 2]
  size.at <- cbind(prices, size.at)[, 2]
  Trade <- cumsum(statechg(states))
  pricesLag <- prices
  pricesLag[which(!size.at & !roll.at)] <- NA
  pricesLag <- pricesLag + (rrices - prices) * as.numeric(roll.at)
  pricesLag <- na.locf(pricesLag)
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
  cbind(States=states, Trade, Size=size.at, Roll=roll.at, Delta=delta, Prices=prices, PnL, RoR, Equity)
}
