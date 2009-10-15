ror <- function(prices, states, delta=1, roll.at=FALSE, percent=TRUE){
  if(percent)
    return((exp(1)^pnl(log(prices), states, roll.at)) * delta - 1)
  else
    pnl(prices, states, roll.at) * delta
}

cumror <- function(prices, states, delta=1, size.at=FALSE, roll.at=FALSE, percent=TRUE){
  PnL <- pnl(prices, states, roll.at)
  size.at <- as.logical(cbind(size.at, PnL)[,1])
  CPnL <- cumsum(PnL)
  CRoR <- CPnL - CPnL[c(1,which.expand(size.at))[-length(size.at)]]
  size.at[1] <- TRUE
  (CRoR + 1) * cumprod(CRoR[which(size.at)] + 1)[match(which.expand(size.at), which(size.at))]
}
