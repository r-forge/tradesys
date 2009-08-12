equity <- function(data, states, delta=1, size.at=as.logical(c(states[1], diff(states))), roll.at=FALSE, percent=TRUE, pricemap=colnames(data)[1]){
  data <- as.matrix(data)
  if(is.null(colnames(data)))
    colnames(data) <- paste("C", seq(1, ncol(data)), sep="")
  ## states, delta, size.at and roll.at length must be multiple of price
  states <- cbind(states, data)[, 1]
  delta <- as.logical(cbind(delta, data)[, 1])
  roll.at <- as.logical(cbind(roll.at, data)[, 1])
  size.at <- as.logical(cbind(size.at, data)[, 1])
  ## process data
  if(any(!pricemap %in% colnames(data)))
    stop("all pricemap pmust be in colnames(data)")
  pricemap <- pricemapper(pricemap)
  data <- prices(data, states, pricemap, roll.at)
  rrices <- as.vector(data[, "Roll"])
  prices <- as.vector(data[, "Price"])
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
