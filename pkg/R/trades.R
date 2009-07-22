trades <- function(x, delta=NULL, uselog=FALSE){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  d <- data.frame(Phase=phases(x), ETime=index(x))
  d <- cbind(d, XTime=NA, Time=NA, Numb=NA, EPrice=NA, XPrice=NA, PnL=NA, RoR=NA)
  d <- d[-which(as.vector(d$Phase) == "UC"),]
  if(d$ETime[nrow(d)] == end(x))
    d <- d[-nrow(d),] ## ignore entries done on last day
  d$XTime <- c(d$ETime[-1], end(x))
  d$EPrice[d$Phase == "EL"] <- x[match(d$ETime[d$Phase == "EL"], index(x)), attr(x, "tsts")$pricecols$enterlong]
  d$EPrice[d$Phase == "ES"] <- x[match(d$ETime[d$Phase == "ES"], index(x)), attr(x, "tsts")$pricecols$entershort]
  d$XPrice[d$Phase == "EL"] <- x[match(d$XTime[d$Phase == "EL"], index(x)), attr(x, "tsts")$pricecols$exitlong]
  d$XPrice[d$Phase == "ES"] <- x[match(d$XTime[d$Phase == "ES"], index(x)), attr(x, "tsts")$pricecols$exitshort]
  if(!is.null(attr(x, "tsts")$roll.at)){ ## create roll trades if needed
    roll.n <- match(attr(x, "tsts")$roll.at, index(x))
    roll.n <- roll.n[which(states(x)[roll.n] != 0)]
    if(length(roll.n) > 0)
      d <- cbind(d, roll=0)
    for(i in roll.n){
      rdate <- index(x)[i]
      n <- which(rdate >= d$ETime & rdate < d$XTime)
      if(d$Phase[n] == "EL"){
        roll.phase <- "EL"
        roll.coli <- attr(x, "tsts")$pricecols$rolllong
        roll.colo <- attr(x, "tsts")$pricecols$exitlong
      }else{
        roll.phase <- "ES"
        roll.coli <- attr(x, "tsts")$pricecols$rollshort
        roll.colo <- attr(x, "tsts")$pricecols$exitshort
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
  d$PnL <- d$XPrice - d$EPrice
  d$PnL[which(d$Phase == "ES")] <- d$PnL[which(d$Phase == "ES")] * -1
  rownames(d) <- as.character(1:nrow(d))
  if(uselog){
    d$RoR <- log(d$XPrice) - log(d$EPrice) 
    d$RoR[which(d$Phase == "ES")] <- d$RoR[which(d$Phase == "ES")] * -1
    if(is.null(delta))
      delta <- OptimalF(d$RoR) / -min(d$RoR)
    d$RoR <- d$RoR * delta
  }else{
    if(is.null(delta))
      delta <- OptimalF(d$PnL) / -min(d$PnL)
    d$RoR <- d$PnL * delta
  }
  d
}
