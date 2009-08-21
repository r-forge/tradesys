equity <- function(prices, states, delta=1, size.at=FALSE, roll.at=FALSE, percent=TRUE){
  ## process prices
  prices <- as.matrix(prices)
  if(ncol(prices) == 1)
    prices <- cbind(prices, prices) 
  Rt <- as.vector(prices[, 2])
  Pt <- as.vector(prices[, 1])
  ## states, delta, size.at and roll.at length must be multiple of price
  states <- cbind(states, Pt)[, 1]
  delta <- cbind(delta, Pt)[, 1]
  size.at <- as.logical(cbind(size.at, Pt)[, 1])
  roll.at <- as.logical(cbind(roll.at, Pt)[, 1])
  ## calculate Pi
  Entries <- TradeEntries(states)
  Entries <- Entries | size.at
  Entries[length(Entries)] <- FALSE ## ignore entry on last obs
  roll.at[length(roll.at)] <- FALSE 
  Exits <- TradeExits(states)
  Pi <- rep(NA, length(Pt))
  Pi[which(Entries) + 1] <- Pt[which(Entries)]
  Pi[which(roll.at) + 1] <- na.locf(Pi, na.rm=FALSE)[which(roll.at)] + Rt[which(roll.at)] - Pt[which(roll.at)]
  Pi <- na.locf(Pi, na.rm=FALSE)
  Pi[is.na(Pi)] <- Pt[is.na(Pi)]
  ## calculate Di
  Di <- rep(NA, length(delta))
  Di[which(Entries) + 1] <- delta[which(Entries)] 
  Di <- na.locf(Di, na.rm=FALSE)
  Di[is.na(Di)] <- delta[is.na(Di)]
  ## calculate HPR
  if(percent) 
    HPR <- (Pt / Pi - 1) * c(0, states[-length(states)]) * Di
  else        
    HPR <- (Pt - Pi) * c(0, states[-length(states)]) * Di
  ## calculate equity at entries: Et
  Et <- rep(NA, length(HPR))
  Et[1] <- 1
  Et[Exits] <- cumprod(HPR[Exits] + 1)
  Et <- na.locf(Et, na.rm=FALSE)
  Et[!Exits] <- Et[!Exits] * (HPR[!Exits] + 1)
  cbind(Trade=TradeID(states), Pt, St=states, HPR, Equity=Et)
}
