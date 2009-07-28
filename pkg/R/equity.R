equity <- function(prices, states, delta=1, size.at=NULL, roll.at=FALSE, roll.prices=prices, percent=TRUE){
  ## all three args must be multiples of each other.. expand to longest
  Prices <- cbind(prices, states, delta)[, 1]
  States <- cbind(prices, states, delta)[, 2]
  Delta <- cbind(prices, states, delta)[, 3]
  Rollat <- cbind(prices, roll.prices, roll.at)[, 3]
  Rollprc <- cbind(prices, roll.prices, roll.at)[, 2]
  Resize <- as.logical(c(abs(States[1]), sapply(abs(diff(States)), min, 1) == 1))
  Trade <- cumsum(Resize)
  if(!is.null(size.at))
    Resize <- cbind(size.at, prices)[, 1]
  PricesLag <- Prices
  PricesLag[which(!Resize&!Rollat)] <- NA
  PricesLag <- PricesLag + (Rollprc - Prices) * as.numeric(Rollat)
  PricesLag <- na.locf(PricesLag)
  PnL <- 0
  RoR <- 0
  if(length(Prices) > 1){
    PnL <- c(PnL, (Prices[2:length(Prices)] - PricesLag[-length(PricesLag)]) * States[-length(States)])
    RoR <- c(RoR, (Prices[2:length(Prices)] / PricesLag[-length(PricesLag)] - 1) * States[-length(States)])
  }
  EquityLag <- rep(1, length(Prices))
  if(percent)
    Equity <- 1 + RoR * Delta
  else
    Equity <- 1 + PnL * Delta
  if(length(Prices) > 1){ ## recursively solve Equity adjusting for resizing
    sapply(2:length(Prices), function(i){
      e <- Equity
      elag <- EquityLag
      e[i] <- Equity[i] * EquityLag[i-1]
      if(Resize[i])
        elag[i] <- e[i]
      else
        elag[i] <- elag[i-1]
      assign("Equity", e, inherits=TRUE)
      assign("EquityLag", elag, inherits=TRUE)
    })
  }
  x <- cbind(States, Trade, Resize, Delta, Prices, PnL, RoR, Equity)
  x
}
