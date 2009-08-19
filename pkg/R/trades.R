trades <- function(prices, states, delta=1, roll.at=FALSE, percent=FALSE, order.by=index(prices)){
  ## process prices
  prices <- as.matrix(prices)
  if(ncol(prices) == 1)
    prices <- cbind(prices, prices) 
  rrices <- as.vector(prices[, 2])
  prices <- as.vector(prices[, 1])
  ## process states
  if(any(!states %in% c(1,0,-1)))
    stop("all states must be in c(1,0,-1)")
  states <- cbind(states, prices)[, 1]
  ## process roll.at
  roll.at <- as.logical(cbind(roll.at, prices)[, 1])
  ## process delta
  if(!is.numeric(delta))
    stop("delta must a be numeric vector")
  delta <- cbind(delta, prices)[, 1]
  d <- as.data.frame(TradeTable(states))
  colnames(d) <- c("Trade","LS","ETime","XTime")
  d[d$XTime > length(states), "XTime"] <- length(states)
  d <- cbind(d, EPrice=prices[d$ETime], XPrice=prices[d$XTime], Delta=delta[d$ETime])
  ## handle roll trades
  if(any(roll.at)){
    Rtm <- which(roll.at)
    Rtr <- sapply(Rtm, function(x) max(which(x >= d$ETime)))
    for(i in seq(1, length(Rtm))){
      if(d[Rtr[i]]$XTime > Rtm[i])
        d$EPrice[Rtr[i]] <- d$EPrice[Rtr[i]] + rrices[Rtm[i]] - prices[Rtm[i]]
    }
  }
  ## calculate pnl and ror
  d <- cbind(d, PnL=(d$XPrice - d$EPrice) * d$LS)
  if(percent)
    d <- cbind(d, RoR=((d$XPrice / d$EPrice) - 1) * d$LS * d$Delta)
  else
    d <- cbind(d, RoR=(d$XPrice - d$EPrice) * d$LS * d$Delta)
  ## duration calcs
  d <- cbind(d, Numb=d$XTime - d$ETime)
  d$ETime <- order.by[d$ETime]
  d$XTime <- order.by[d$XTime]
  d <- cbind(d, Time=as.numeric(d$XTime - d$ETime))
  d[, c("Trade","LS","ETime","XTime","Numb","Time","EPrice","XPrice","Delta","PnL","RoR")]
}
