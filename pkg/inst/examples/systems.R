library(tradesys)
library(TTR)

##
## 60/120 Moving Average Crossover System
##
## When the 60-day MA crosses over the 120-day MA, go long at the following day's open.
## When the 60-day MA crosses under the 120-day MA, go short at the following day's open.

x <- spx[, c("Open","Close")]
d <- expr.frame(x, list(MAf=quote(SMA(Close, 60)), MAs=quote(SMA(Close, 120))))
d$Cross <- quote(c(0,diff(MAf > MAs)))
d$St <- quote(signalmap(el=Cross == 1, es=Cross == -1))
d$Price <- quote(prices(St, uc=Close, chg=c(Open[-1], NA)))
d$PnL <- quote(pnl(Price, St))
d$RoR <- quote(ror(Price, St, delta=1/Price))

## System Logic
exprlist(d)

## Plot Equity curve
plot(d$RoR)

## Compound Annualised Return
d$RoR[length(d$RoR)]^(260/length(d$RoR)) - 1

## Explore..
head(as.data.frame(d))
tail(as.data.frame(d))
