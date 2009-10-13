test.pnl <- function(){

  ## These are the Close prices for the second and third Eurodollar series.
  ## The contract rolled on 06/15, so ED2 represents EDU9 until 06/16, at
  ## which time it starts representing EDZ9. (Similarly, ED3 represents EDZ9 before
  ## and EDH0 after.) 

  ##               ED02    ED03
  ## 1 2009-06-10 99.210  98.790
  ## 2 2009-06-11 99.240  98.850
  ## 3 2009-06-12 99.265  98.900
  ## 4 2009-06-15 99.265  98.930
  ## 5 2009-06-16 98.935  98.635
  ## 6 2009-06-17 98.945  98.670
  ## 7 2009-06-18 98.840  98.525
  ## 8 2009-06-19 98.935  98.630

  ED <- cbind(c(99.210,99.240,99.265,99.265,98.935,98.945,98.840,98.935),
              c(98.790,98.850,98.900,98.930,98.635,98.670,98.525,98.630))

  colnames(ED) <- c("ED02","ED03")

  ## The roll is at row 4

  roll.at <- c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)

  ## Long throughout
  checkEquals(pnl(ED, rep(1, 8), roll.at), c(0,.03,.025,0,98.935 - (99.265 + 98.930 - 99.265),.01,-.105,.095))

  ## Flat throughout
  checkEquals(pnl(ED, 0, roll.at), rep(0, 8))

  ## One period case
  checkEquals(pnl(10,1), 0)
  checkEquals(pnl(10,0), 0)
  checkEquals(pnl(10,-1), 0)

  ## Flat except for final period
  checkEquals(pnl(c(10,13), c(0,1)), c(0,0))
  checkEquals(pnl(c(10,13), c(0,-1)), c(0,0))
  
  ## Misc.
  checkEquals(sum(pnl(rep(c(100,110), 10), rep(c(1,-1), 10))), 190)
  checkEquals(sum(pnl(rep(c(110,100), 10), rep(c(1,-1), 10))), -190)

  ## Error: "states must consist of 1, 0, and -1"
  checkException(pnl(ED, c(0,1,1,1,1,pi,1,1)), silent=TRUE)

  ## Error: "roll.at must be a logical vector"
  checkException(pnl(ED, c(0,1,1,1,1,1,1,1), c(0,1,1,1,1,pi,1,1)), silent=TRUE)

  ## Error: "length(states) must be a multiple of nrow(prices)"
  checkException(pnl(10:13, c(1,1,1)), silent=TRUE)

  ## Error: "length(roll.at) must be a multiple of nrow(prices)"
  checkException(pnl(10:13, c(1,1,1,1), c(TRUE,TRUE,FALSE)), silent=TRUE)

}
