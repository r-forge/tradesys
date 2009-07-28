equity <- function(prices, states, delta=1, size.at=NULL, roll.at=FALSE, percent=TRUE){
  if(is.matrix(prices)){
    Prices <- prices[, 1]
    Rrices <- prices[, 2]
  }else{
    Prices <- prices
    Rrices <- prices
  }
  ## states, delta, size.at and roll.at be multiples of price
  States <- cbind(prices, states)[, 2]
  Delta <- cbind(prices, delta)[, 2]
  Rollat <- cbind(prices, roll.at)[, 2]
  Sizeat <- as.logical(c(abs(States[1]), sapply(abs(diff(States)), min, 1) == 1))
  Trade <- cumsum(Sizeat)
  if(!is.null(size.at))
    Sizeat <- cbind(size.at, prices)[, 1]
  PricesLag <- Prices
  PricesLag[which(!Sizeat&!Rollat)] <- NA
  PricesLag <- PricesLag + (Rrices - Prices) * as.numeric(Rollat)
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
      if(Sizeat[i])
        elag[i] <- e[i]
      else
        elag[i] <- elag[i-1]
      assign("Equity", e, inherits=TRUE)
      assign("EquityLag", elag, inherits=TRUE)
    })
  }
  x <- cbind(States, Trade, Sizeat, Delta, Prices, PnL, RoR, Equity)
  x
}
