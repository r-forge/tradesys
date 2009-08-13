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
  d <- data.frame(Trade=cumsum(as.logical(c(states[1], diff(states)))),
                  Phase=phasemap(states), ETime=order.by, EPrice=prices)
  d <- cbind(d, XTime=NA, Time=NA, Numb=NA, XPrice=NA, PnL=NA, RoR=NA)
  d <- d[-which(as.vector(d$Phase) == "UC"),]
  if(all(states == 0)) ## return empty df if no trades
    return(d)
  if(d$ETime[nrow(d)] == order.by[length(order.by)])
    d <- d[-nrow(d),] ## ignore entries done on last day
  d$XTime <- c(d$ETime[-1], order.by[length(order.by)])
  d$XPrice[match(d$XTime, order.by)] <- prices[match(d$XTime, order.by)]
  d <- d[d$Phase %in% c("EL","ES"), ] ## remove exit rows
  if(any(roll.at)){ ## create roll trades if needed
    d <- cbind(d, roll=0)
    for(i in which(roll.at)){
      ## A roll trade is insert between an entry and exit if roll date is on
      ## or after entry time and before exit time.
      if(length(n <- which(order.by[i] >= d$ETime & order.by[i] < d$XTime)) > 0){
        ## roll-in trade
        dd <- data.frame(Phase=d$Phase[n], ETime=order.by[i], XTime=d$XTime[n], Time=NA, Numb=NA,
                         EPrice=rrices[i], XPrice=d$XPrice[n], PnL=NA, RoR=NA, roll=1)
        ## roll-out trade
        d$XTime[n] <- order.by[i]    
        d$XPrice[n] <- prices[i]
        d$roll[n] <- -1
        d <- rbind(d[1:n,], dd, d[(n+1):nrow(d),])
      }
    }
  }
  ## duration calcs
  d$Time <- as.integer(d$XTime - d$ETime)
  d$Numb <- match(d$XTime, order.by) - match(d$ETime, order.by)
  ## PnL/RoR calcs
  d$PnL <- (d$XPrice - d$EPrice) * c(1,-1)[match(d$Phase, c("EL","ES"))]
  if(percent)
    d$RoR <- (d$XPrice / d$EPrice - 1) * c(1,-1)[match(d$Phase, c("EL","ES"))]
  else
    d$RoR <- d$PnL
  d$RoR <- d$RoR * delta
  rownames(d) <- as.character(1:nrow(d))
  d
}
