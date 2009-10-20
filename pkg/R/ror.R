ror <- function(prices, states, roll.at=FALSE, delta=1, size.at=TRUE){
  RoR <- pnl(prices, states, roll.at) 
  if(nrow(as.matrix(prices)) %% length(delta) != 0)
    stop("length(delta) must be a multiple of nrow(prices)")
  else
    delta <- cbind(delta, RoR)[,1]
  if(nrow(as.matrix(prices)) %% length(size.at) != 0)
    stop("length(size.at) must be a multiple of nrow(prices)")
  else
    size.at <- as.logical(cbind(size.at, RoR)[,1])
  RoR[-1] <- RoR[-1] * delta[which.expand(size.at)][-length(delta)]
  CRoR <- cumsum(RoR)
  CRoR[-1] <- CRoR[-1] - CRoR[which.expand(size.at)[-length(size.at)]]
  size.at[1] <- TRUE
  n <- c(1, match(which.expand(size.at), which(size.at))[-length(size.at)])
  (CRoR + 1) * cumprod(CRoR[which(size.at)] + 1)[n]
}


