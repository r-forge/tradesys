equity <- function(prices, states, delta=1, size.at=as.logical(c(states[1], diff(states))), roll.at=FALSE, percent=TRUE){
  ## process prices
  prices <- as.matrix(prices)
  if(ncol(prices) == 1)
    prices <- cbind(prices, prices) 
  Rt <- as.vector(prices[, 2])
  Pt <- as.vector(prices[, 1])
  ## states, delta, size.at and roll.at length must be multiple of price
  states <- cbind(states, Pt)[, 1]
  delta <- cbind(delta, Pt)[, 1]
  roll.at <- as.logical(cbind(roll.at, Pt)[, 1])
  size.at <- as.logical(cbind(size.at, Pt)[, 1])
  ## calculate P(i)
  n <- length(Pt)
  Pi <- rep(NA, n)
  Pi[1] <- Pt[1]
  Pi[size.at] <- Pt[size.at]
  Pi <- na.locf(Pi, na.rm=FALSE)
  Pi <- Pi + (Rt - Pt) * as.numeric(roll.at)
  Pi[!size.at & !roll.at] <- NA
  Pi <- na.locf(Pi, na.rm=FALSE)
  PnL <- 0
  RoR <- 0
  if(n > 1){
    PnL <- c(PnL, (Pt[2:n] - Pi[-n]) * states[-n])
    RoR <- c(RoR, (Pt[2:n] / Pi[-n] - 1) * states[-n])
  }
  Ei <- rep(1, n)
  if(percent)
    Et <- 1 + RoR * delta
  else
    Et <- 1 + PnL * delta
  if(n > 1){ ## recursively solve E(t) adjusting for resizing
    sapply(2:n, function(i){
      e <- Et
      elag <- Ei
      e[i] <- Et[i] * Ei[i-1]
      if(size.at[i])
        elag[i] <- e[i]
      else
        elag[i] <- elag[i-1]
      assign("Et", e, inherits=TRUE)
      assign("Ei", elag, inherits=TRUE)
    })
  }
  cbind(States=states, Trade=cumsum(as.logical(c(states[1], diff(states)))),
        Size=size.at, Roll=roll.at, Delta=delta, Prices=Pt, Rt, Pi, PnL, RoR, Equity=Et)
}
