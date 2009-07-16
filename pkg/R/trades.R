trades <- function(x, delta=NULL, uselog=FALSE){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  d <- data.frame(phase=phases(x), etime=index(x))
  d <- cbind(d, xtime=NA, time=NA, nobs=NA, eprice=NA, xprice=NA, pnl=NA, ror=NA)
  d <- d[-which(as.vector(d$phase) == "UC"),]
  if(d$etime[nrow(d)] == end(x))
    d <- d[-nrow(d),] ## ignore entries done on last day
  d$xtime <- c(d$etime[-1], end(x))
  d$eprice[d$phase == "EL"] <- x[match(d$etime[d$phase == "EL"], index(x)), attr(x, "tsts")$pricecols$enterlong]
  d$eprice[d$phase == "ES"] <- x[match(d$etime[d$phase == "ES"], index(x)), attr(x, "tsts")$pricecols$entershort]
  d$xprice[d$phase == "EL"] <- x[match(d$xtime[d$phase == "EL"], index(x)), attr(x, "tsts")$pricecols$exitlong]
  d$xprice[d$phase == "ES"] <- x[match(d$xtime[d$phase == "ES"], index(x)), attr(x, "tsts")$pricecols$exitshort]
  if(!is.null(attr(x, "tsts")$roll.at)){ ## create roll trades if needed
    roll.n <- match(attr(x, "tsts")$roll.at, index(x))
    roll.n <- roll.n[which(states(x)[roll.n] != 0)]
    if(length(roll.n) > 0)
      d <- cbind(d, roll=0)
    for(i in roll.n){
      rdate <- index(x)[i]
      n <- which(rdate >= d$etime & rdate < d$xtime)
      if(d$phase[n] == "EL"){
        roll.phase <- "EL"
        roll.coli <- attr(x, "tsts")$pricecols$rolllong
        roll.colo <- attr(x, "tsts")$pricecols$exitlong
      }else{
        roll.phase <- "ES"
        roll.coli <- attr(x, "tsts")$pricecols$rollshort
        roll.colo <- attr(x, "tsts")$pricecols$exitshort
      }
      ## roll-in trade
      dd <- data.frame(phase=roll.phase, etime=rdate, xtime=d$xtime[n], time=NA, nobs=NA,
                       eprice=x[i, roll.coli], xprice=d$xprice[n], pnl=NA, ror=NA, roll=1)
      ## roll-out trade
      d$xtime[n] <- rdate    
      d$xprice[n] <- x[i, roll.colo]
      d$roll[n] <- -1
      d <- rbind(d[1:n,], dd, d[(n+1):nrow(d),])
    }
  }
  ## duration calcs
  d$time <- as.integer(d$xtime - d$etime)
  d$nobs <- match(d$xtime, index(x)) - match(d$etime, index(x))
  ## pnl/ror calcs
  d$pnl <- d$xprice - d$eprice
  d$pnl[which(d$phase == "ES")] <- d$pnl[which(d$phase == "ES")] * -1
  rownames(d) <- as.character(1:nrow(d))
  if(uselog){
    d$ror <- log(d$xprice) - log(d$eprice) 
    d$ror[which(d$phase == "ES")] <- d$ror[which(d$phase == "ES")] * -1
    if(is.null(delta))
      delta <- OptimalF(d$ror) / -min(d$ror)
    d$ror <- d$ror * delta
  }else{
    if(is.null(delta))
      delta <- OptimalF(d$pnl) / -min(d$pnl)
    d$ror <- d$pnl * delta
  }
  d
}
