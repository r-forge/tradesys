equity <- function(x, delta=NULL, uselog=FALSE){
  h <- phases(x)
  s <- states(x)
  p <- as.matrix(x)[, attr(x, "tsts")$pricecols$valuation]
  q <- as.matrix(x)[, attr(x, "tsts")$pricecols$rolllong]
  q[which(s == -1)] <- as.matrix(x)[which(s == -1), attr(x, "tsts")$pricecols$rollshort]
  p[which(h == "EL")] <- as.matrix(x)[which(h == "EL"), attr(x, "tsts")$pricecols$enterlong]
  p[which(h == "ES")] <- as.matrix(x)[which(h == "ES"), attr(x, "tsts")$pricecols$entershort]
  p[which(h == "XL")] <- as.matrix(x)[which(h == "XL"), attr(x, "tsts")$pricecols$exitlong]
  p[which(h == "XS")] <- as.matrix(x)[which(h == "XS"), attr(x, "tsts")$pricecols$exitshort]
  if(attr(x, "tsts")$pricecols$entershort != attr(x, "tsts")$pricecols$exitlong)
    message("The entershort price column will be used for XL phases where s(t)=-1 and s(t-1)=1.")
  if(attr(x, "tsts")$pricecols$enterlong != attr(x, "tsts")$pricecols$exitshort)
    message("The enterlong price column will be used for XS phases where s(t)=1 and s(t-1)=-1.")
  if(uselog)
    p <- log(p)
  pnl <- c(0, diff(p) * s[-length(s)])
  if(is.null(delta))
    delta <- OptimalF(pnl) / -min(pnl)
  d <- delta
  r <- 0
  n <- which(h %in% c("EL","ES"))
  m <- match(attr(x, "tsts")$roll.at, index(x))
  for(i in 2:length(pnl)){
    if(i %in% (n+1))
      d[i] <- delta
    else
      d[i] <- d[i-1] / (1 + r[i-1])
    if(i %in% (m+1)){
      if(s[i-1] == 1)
        p[i] <- as.matrix(x)[i, attr(x, "tsts")$pricecols$rolllong]
      else
        p[i] <- as.matrix(x)[i, attr(x, "tsts")$pricecols$rollshort]
    }
    r[i] <- pnl[i] * d[i]
  }
  y <- cbind(Trade=NA, St=s, Delta=d, Price=p, RoR=r, Equity=cumprod(1+r))
  ## crate trade id's
  y[(n+1), "Trade"] <- 1:length(n)
  y[, "Trade"] <- na.locf(y[, "Trade"], na.rm=FALSE)
  y[which(states(x) == 0), "Trade"] <- 0
  zoo(y, order.by=index(x))
}
