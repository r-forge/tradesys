test.prices <- function(){

  ##
  ## pricecols processing
  ##
  
  ## Member of pricecols not in colnames(x)
  checkException(prices(matrix(1:10, ncol=2, dimnames=list(NULL, c("Open","Close"))), 0, c(Mark="Closed")), silent=TRUE)
  checkException(prices(matrix(1:10, ncol=2, dimnames=list(NULL, c("Open","Close"))), 0, c(Mark="Closed",Long="Open")), silent=TRUE)

  ## Invalid member in names(pricecols) 
  checkException(prices(matrix(1:10, ncol=2, dimnames=list(NULL, c("Open","Close"))), 0, c(Marker="Close",Long="Open")), silent=TRUE)

  ## No 'Mark' in non-null names(pricecols)
  checkException(prices(matrix(1:10, ncol=2, dimnames=list(NULL, c("Open","Close"))), 0, c(Short="Close",Long="Open")), silent=TRUE)

  ## If supplied in pricemap, column gets from x what pricemap says it gets.
  x <- matrix(1:50, ncol=5, dimnames=list(NULL, paste("C", 1:5, sep="")))
  y <- prices(x, 0, c(Mark="C1", Long="C2"))
  checkEquals(y[, "Long"], x[, "C2"], checkNames=FALSE)
  y <- prices(x, 0, c(Mark="C1", Short="C3"))
  checkEquals(y[, "Short"], x[, "C3"], checkNames=FALSE)
  y <- prices(x, 0, c(Mark="C1", RollLong="C4"))
  checkEquals(y[, "RollLong"], x[, "C4"], checkNames=FALSE)
  y <- prices(x, 0, c(Mark="C1", RollShort="C5"))
  checkEquals(y[, "RollShort"], x[, "C5"], checkNames=FALSE)
  
  ## If missing, Long and Short are keyed to 'Mark'
  y <- prices(x, 0, c(Mark="C1", Long="C2"))
  checkEquals(y[, "Short"], y[, "Mark"])
  y <- prices(x, 0, c(Mark="C1", Short="C2"))
  checkEquals(y[, "Long"], y[, "Mark"])

  ## If missing, RollLong (RollShort) are keyed to Long (Short)
  y <- prices(x, 0, c(Mark="C1", Long="C2", Short="C3"))
  checkEquals(y[, "RollLong"], y[, "Long"])
  checkEquals(y[, "RollShort"], y[, "Short"])

  ## All five get the same value if a scalar is passed
  y <- prices(x, 0, "C4")
  checkTrue(all(apply(y, 2, function(x) y[, 1] == x)))

  
  ##
  ## Price and Mark mapping via states
  ##

  Phases <- phasemap(States <- c(1,1,0,0,-1,-1,1,1,0,0))
  y <- prices(x, States, c(Mark="C1", Long="C2", Short="C3", RollLong="C4", RollShort="C5"))
  
  ## Price gets 'Long' when phase is long entry or short exit 
  checkEquals(y[Phases %in% c("EL","XS"), "Price"], x[Phases %in% c("EL","XS"), "C2"])

  ## Price gets 'Short' when phase is short entry or long exit
  checkEquals(y[Phases %in% c("ES","XL"), "Price"], x[Phases %in% c("ES","XL"), "C3"])

  ## RollPrice gets 'RollLong' when state is 1 or 0
  checkEquals(y[States == 1, "RollPrice"], x[States == 1, "C4"])
  checkEquals(y[States == 0, "RollPrice"], x[States == 0, "C4"])
  
  ## RollPrice gets 'RollShort' when state is -1
  checkEquals(y[States == -1, "RollPrice"], x[States == -1, "C5"])
}
