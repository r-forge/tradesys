test.tradesys.frame <- function(){
  x <- tradesys(c("Close"))
  x$el <- quote(SMA(Close, 60) >= SMA(Close, 120))
  x$es <- quote(SMA(Close, 60) < SMA(Close, 120))
  x$xl <- quote(rnorm(Close) > 1.5) ## random exits
  x$el <- quote(rnorm(Close) > 1.5)
  y <- tradesys.frame(x, spx)

  ## Trade column gets new value when and only when trade entry or exit
  checkEquals(c(FALSE, diff(y$Trade) != 0), TradeEntries(y$States) | TradeExits(y$States))

}
