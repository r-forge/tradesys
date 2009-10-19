ror <- function(prices, states, delta=1, size.at=FALSE, roll.at=FALSE, period=FALSE){
  RoR <- pnl(prices, states, roll.at) 
  delta <- cbind(delta, RoR)[,1]
  size.at <- as.logical(cbind(size.at, RoR)[,1])
  RoR[-1] <- RoR[-1] * delta[which.expand(size.at)][-length(delta)]
  CRoR <- cumsum(RoR)
  CRoR[-1] <- CRoR[-1] - CRoR[which.expand(size.at)[-length(size.at)]]
  size.at[1] <- TRUE
  n <- c(1, match(which.expand(size.at), which(size.at))[-length(size.at)])
  x <- (CRoR + 1) * cumprod(CRoR[which(size.at)] + 1)[n]
  if(period){
    x <- c(0, x[2:length(x)] / x[-length(x)] - 1)
  }
  x
}


