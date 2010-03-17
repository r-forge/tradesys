test.prices <- function(){

  s <- sample(c(1,0,-1), 100, TRUE)
  spx <- spx[1:100,]

  ## Pass uc/el/es/xl/xs its corresponding phasemap code 
  (p <- prices(s, rep(0, 100), el=rep(8, 100), es=rep(4, 100), xl=rep(2, 100), xs=rep(1, 100)))
  checkIdentical(phasemap(s), p)

  ## Example 1: entries/exits at Open, unchanged at Close.
  ex1 <- prices(s, spx$Close, chg=spx$Open)
  
  ## Example 2: entries/exits at next day's Open, unchanged at Close.
  ex2 <- prices(s, spx$Close, chg=c(spx$Open[-1], NA))

  ## Example 3: same as above, but with 10bps slippage at entries/exits.
  ex3 <- prices(s, spx$Close, lng=c(spx$Open[-1], NA) * 1.001, sht=c(spx$Open[-1], NA) * .999)
  
  checkIdentical(round(ex3 - ex2, 14), round(ex2 * .001 * changemap(s), 14))

  ################################################################
  ## Why the rounding error at the 15th dec. place?
  ## checkIdentical(ex3 - ex2, ex2 * .001 * changemap(s))
  ################################################################

}
