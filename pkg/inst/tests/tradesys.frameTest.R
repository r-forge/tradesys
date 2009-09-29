test.tradesys.frame <- function(){

  ##
  ## Test all 48 s(t), s(t-1) possibilities
  ##

  m <- matrix(1, nrow=2, dimnames=list(NULL, "Foo"))
  
  ##  0 0000
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(-1,-1))
  ##  1 0001
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(-1,0))
  ##  2 0010
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(-1,-1))
  ##  3 0011
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(-1,0))
  ##  4 0100
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(0,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(1,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(-1,-1))
  ##  5 0101
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(-1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(0,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(1,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(-1,-1)) ##  6 0110
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(0,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(1,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(-1,-1))
  ##  7 0111
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(-1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(0,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, FALSE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(1,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,FALSE), es=c(TRUE, TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(-1,-1))
  ##  8 1000
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(0,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(-1,1))
  ##  9 1001
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(0,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, FALSE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(-1,1))
  ## 10 1010
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(-1,-1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE), entrywins=TRUE), m)$States, c(0,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE), entrywins=TRUE), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE), entrywins=TRUE), m)$States, c(-1,1))
  ## 11 1011
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(-1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(0,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, FALSE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE), entrywins=TRUE), m)$States, c(-1,1))
  ## 12 1100
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,FALSE)), m)$States, c(-1,-1))
  ## 13 1101
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(1,1))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, TRUE), xl=c(FALSE,FALSE), xs=c(FALSE,TRUE)), m)$States, c(-1,0))
  ## 14 1110
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,FALSE)), m)$States, c(-1,-1))
  ## 15 1111
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(0,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(TRUE, TRUE), es=c(FALSE,TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(1,0))
  checkEquals(tradesys.frame(tradesys("Foo", el=c(FALSE,TRUE), es=c(TRUE, TRUE), xl=c(FALSE,TRUE), xs=c(FALSE,TRUE)), m)$States, c(-1,0))

  library(TTR)
  x <- tradesys(c("Close"))
  x$el <- quote(SMA(Close, 60) >= SMA(Close, 120))
  x$es <- quote(SMA(Close, 60) < SMA(Close, 120))
  x$xl <- quote(rnorm(Close) > 1.5) ## random exits
  x$el <- quote(rnorm(Close) > 1.5)

  y <- tradesys.frame(x, spx)

  ## Trade variable gets new value when and only when trade entry or exit
  checkEquals(c(FALSE, diff(y$Trade) != 0), TradeEntries(y$States) | TradeExits(y$States))
}

