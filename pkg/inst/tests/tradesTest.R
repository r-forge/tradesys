test.trades <- function(){
  checkEquals(trades(c(100,110,100,55), c(1,-1,-1,-1), delta=1)$HPR, c(0.1,.5))
  checkEquals(trades(c(100,110,100,55), c(1,-1,-1,-1), delta=.5)$HPR, c(0.05,.25))
  checkEquals(trades(c(100,110,100,55), c(1,-1,-1,-1), delta=2)$HPR, c(0.2,1))
  checkEquals(trades(c(100,110,100,55), c(1,-1,-1,1), delta=c(1,2,3,4))$Delta, c(1,2,4)) 
  checkEquals(trades(c(100,110,100,55), c(1,1,1,1))$HPR, -.45) 
}
