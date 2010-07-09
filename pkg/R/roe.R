roe <- function(prices, states=1, roll.at=FALSE, size.at=TRUE, delta=1/prices){

  ROE <- pnl(prices, states, roll.at) 

  if(nrow(as.matrix(prices)) %% length(delta) != 0){
    stop("length(delta) must be a multiple of nrow(prices)")
  }else{
    delta <- cbind(delta, ROE)[,1]
  }

  if(nrow(as.matrix(prices)) %% length(size.at) != 0){
    stop("length(size.at) must be a multiple of nrow(prices)")
  }else{
    size.at <- as.logical(cbind(size.at, ROE)[,1])
  }

  ROE[-1] <- ROE[-1] * delta[which.expand(size.at)][-length(delta)]
  CUM <- cumsum(ROE)
  CUM[-1] <- CUM[-1] - CUM[which.expand(size.at)[-length(size.at)]]
  size.at[1] <- TRUE
  n <- c(1, match(which.expand(size.at), which(size.at))[-length(size.at)])
  c(1, exp(diff(log((CUM + 1) * cumprod(CUM[which(size.at)] + 1)[n]) + 1))) - 1
}

##roe(c(100,110,100,90)) + 1
##c(1,exp(diff(log(c(100,110,100,90)))))

