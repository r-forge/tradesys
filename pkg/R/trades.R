trades <- function(x, delta=1, percent=FALSE, states=states(x), pricecols=pricecols(x), roll.at=FALSE){
  d <- data.frame(Phase=phases(x), ETime=index(x))
  d <- cbind(d, XTime=NA, Time=NA, Numb=NA, EPrice=NA, XPrice=NA, PnL=NA, RoR=NA)
  d <- d[-which(as.vector(d$Phase) == "UC"),]
  if(d$ETime[nrow(d)] == end(x))
    d <- d[-nrow(d),] ## ignore entries done on last day
  d$XTime <- c(d$ETime[-1], end(x))
  d$EPrice[d$Phase == "EL"] <- x[match(d$ETime[d$Phase == "EL"], index(x)), pricecols$Long]
  d$EPrice[d$Phase == "ES"] <- x[match(d$ETime[d$Phase == "ES"], index(x)), pricecols$Short]
  d$XPrice[d$Phase == "EL"] <- x[match(d$XTime[d$Phase == "EL"], index(x)), pricecols$Short]
  d$XPrice[d$Phase == "ES"] <- x[match(d$XTime[d$Phase == "ES"], index(x)), pricecols$Long]
  if(any(roll.at)){ ## create roll trades if needed
    roll.n <- match(attr(x, "tsts")$roll.at, index(x))
    roll.n <- roll.n[which(states(x)[roll.n] != 0)]
    if(length(roll.n) > 0)
      d <- cbind(d, roll=0)
    for(i in roll.n){
      rdate <- index(x)[i]
      n <- which(rdate >= d$ETime & rdate < d$XTime)
      if(d$Phase[n] == "EL"){
        roll.phase <- "EL"
        roll.coli <- attr(x, "tsts")$pricecols$RollLong
        roll.colo <- attr(x, "tsts")$pricecols$Short
      }else{
        roll.phase <- "ES"
        roll.coli <- attr(x, "tsts")$pricecols$RollShort
        roll.colo <- attr(x, "tsts")$pricecols$Long
      }
      ## roll-in trade
      dd <- data.frame(Phase=roll.phase, ETime=rdate, XTime=d$XTime[n], Time=NA, Numb=NA,
                       EPrice=x[i, roll.coli], XPrice=d$XPrice[n], PnL=NA, RoR=NA, roll=1)
      ## roll-out trade
      d$XTime[n] <- rdate    
      d$XPrice[n] <- x[i, roll.colo]
      d$roll[n] <- -1
      d <- rbind(d[1:n,], dd, d[(n+1):nrow(d),])
    }
  }
  ## duration calcs
  d$Time <- as.integer(d$XTime - d$ETime)
  d$Numb <- match(d$XTime, index(x)) - match(d$ETime, index(x))
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
