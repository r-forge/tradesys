trades <- function(data, states, delta=1, roll.at=FALSE, percent=FALSE, order.by=index(data), pricemap=colnames(data)[1]){
  ## process pricemap
  if(any(!pricemap %in% colnames(data)))
    stop("all pricemap must be in colnames(data)")
  pricemap <- pricemapper(pricemap)
  ## process states
  if(any(!states %in% c(1,0,-1)))
    stop("states must consist of 1s, 0s, and -1s")
  ## process delta
  if(!is.numeric(delta))
    stop("states must a be numeric vector")
  d <- data.frame(Phase=phases(states), ETime=order.by)
  d <- cbind(d, XTime=NA, Time=NA, Numb=NA, EPrice=NA, XPrice=NA, PnL=NA, RoR=NA)
  d <- d[-which(as.vector(d$Phase) == "UC"),]
  if(all(states == 0)){ ## return empty df if no trades
    return(d)
  }
  if(d$ETime[nrow(d)] == end(data))
    d <- d[-nrow(d),] ## ignore entries done on last day
  d$XTime <- c(d$ETime[-1], end(data))
  d$EPrice[d$Phase == "EL"] <- data[match(d$ETime[d$Phase == "EL"], order.by), pricemap["Long"]]
  d$EPrice[d$Phase == "ES"] <- data[match(d$ETime[d$Phase == "ES"], order.by), pricemap["Short"]]
  d$XPrice[d$Phase == "EL"] <- data[match(d$XTime[d$Phase == "EL"], order.by), pricemap["Short"]]
  d$XPrice[d$Phase == "ES"] <- data[match(d$XTime[d$Phase == "ES"], order.by), pricemap["Long"]]
  if(any(roll.at)){ ## create roll trades if needed
    roll.n <- match(roll.at, order.by)
    roll.n <- roll.n[which(states[roll.n] != 0)]
    if(length(roll.n) > 0)
      d <- cbind(d, roll=0)
    for(i in roll.n){
      rdate <- order.by[i]
      n <- which(rdate >= d$ETime & rdate < d$XTime)
      if(d$Phase[n] == "EL"){
        roll.phase <- "EL"
        roll.coli <- pricemap["RollLong"]
        roll.colo <- pricemap["Short"]
      }else{
        roll.phase <- "ES"
        roll.coli <- pricemap["RollShort"]
        roll.colo <- pricemap["Long"]
      }
      ## roll-in trade
      dd <- data.frame(Phase=roll.phase, ETime=rdate, XTime=d$XTime[n], Time=NA, Numb=NA,
                       EPrice=data[i, roll.coli], XPrice=d$XPrice[n], PnL=NA, RoR=NA, roll=1)
      ## roll-out trade
      d$XTime[n] <- rdate    
      d$XPrice[n] <- data[i, roll.colo]
      d$roll[n] <- -1
      d <- rbind(d[1:n,], dd, d[(n+1):nrow(d),])
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
