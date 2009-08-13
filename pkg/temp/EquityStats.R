EquityStats <- function(equity, dates=index(equity)){
  if(class(dates) != "Date")
    stop("dates must be class 'Date'")
  E <- coredata(equity)
  D <- as.numeric(dates[length(dates)] - dates[1])
  ROR <- c(0, E[2:length(E)] / E[1:(length(E) - 1)] - 1)
  RORC <- E[length(E)] / E[1] - 1
  CAGR <- (E[length(E)] / E[1])^(365 / D) - 1
  x <- 1:length(E)
  y <- coredata(E) / coredata(E)[1] - 1
  LM <- summary(lm(y ~ x))
  RORP <- as.vector(LM$coefficients[, 2])
  RORP <- RORP[2] * 365 + RORP[1]
  VOLA <- sd(ROR, na.rm=TRUE) * sqrt(D / 365)
  MAXDD <- min(equity / cummax(E) - 1)
  R2 <- LM$r.squared
  names(RORC) <- NULL
  names(CAGR) <- NULL
  names(RORP) <- NULL
  names(R2) <- NULL
  names(VOLA) <- NULL
  names(MAXDD) <- NULL
  round(c(RORC=RORC, CAGR=CAGR, "ROR%"=RORP, R2=R2, VOLA=VOLA, MAXDD=MAXDD), 5)
}

