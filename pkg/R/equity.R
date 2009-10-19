##
## !!! DEPRECATED !!! 
##

equity <- function(prices, states, delta=1, size.at=FALSE, roll.at=FALSE, percent=TRUE, init=1){
  ## process args
  states <- as.vector(states)
  size.at <- as.vector(size.at)
  roll.at <- as.vector(roll.at)
  percent <- percent[1]
  init <- init[1]
  prices <- as.matrix(prices)
  if(ncol(prices) == 1)
    prices <- cbind(prices, prices, prices)
  It <- as.vector(prices[, 3])
  Ot <- as.vector(prices[, 2])
  Pt <- as.vector(prices[, 1])
  ## proces states, delta, size.at and roll.at
  states <- cbind(states, Pt)[, 1]
  delta <- cbind(delta, Pt)[, 1]
  size.at <- as.logical(cbind(size.at, Pt)[, 1])
  roll.at <- as.logical(cbind(roll.at, Pt)[, 1])
  size.at <- TradeEntries(states) | TradeExits(states) | size.at
  size.at[length(size.at)] <- FALSE ## ignore TRUE on last obs
  roll.at[length(roll.at)] <- FALSE
  RollAdj <- (It - Ot) * abs(states) * as.numeric(roll.at)
  ## calculate Pi
  Pi <- rep(NA, length(Pt))
  Pi[which(size.at) + 1] <- Pt[which(size.at)]
  for(i in which(roll.at))
    Pi[i + 1] <- na.locf(Pi, na.rm=FALSE)[i] + RollAdj[i] 
  Pi <- na.locf(Pi, na.rm=FALSE)
  Pi[is.na(Pi)] <- Pt[is.na(Pi)]
  ## calculate Di
  Di <- rep(NA, length(delta))
  Di[which(size.at) + 1] <- delta[which(size.at)] 
  Di <- na.locf(Di, na.rm=FALSE)
  Di[is.na(Di)] <- delta[is.na(Di)]
  ## calculate HPR
  if(percent) 
    HPR <- (Pt / Pi - 1) * c(0, states[-length(states)]) * Di
  else        
    HPR <- (Pt - Pi) * c(0, states[-length(states)]) * Di
  ## calculate Et (equity at size.at times)
  Et <- rep(NA, length(HPR))
  Et[1] <- init
  Et[size.at] <- cumprod(HPR[size.at] + 1) * Et[1]
  Et <- na.locf(Et, na.rm=FALSE)
  Et[!size.at] <- Et[!size.at] * (HPR[!size.at] + 1)
  cbind(Trade=TradeID(states), Price=Pt, RollAdj, States=states, Delta=Di, Size=size.at, Roll=roll.at, HPR, Equity=Et)
}
