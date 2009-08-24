test.roll <- function(){

  ## These are the closing Bid/Ask/Close prices for the second and third Eurodollar
  ## series. The contract rolled on 06/15, so ED2 represents EDU9 until 06/16, at
  ## which time it starts representing EDZ9. (Similarly, ED3 represents EDZ9 before
  ## and EDH0 after.) 

  ##              ED2.BID ED2.ASK ED2.CLS ED3.BID ED3.ASK ED3.CLS
  ## 1 2009-06-10  99.190  99.220  99.210  98.770  98.795  98.790
  ## 2 2009-06-11  99.230  99.250  99.240  98.835  98.880  98.850
  ## 3 2009-06-12  99.235  99.275  99.265  98.880  98.925  98.900
  ## 4 2009-06-15  99.245  99.285  99.265  98.930  98.945  98.930
  ## 5 2009-06-16  98.950  98.955  98.935  98.660  98.670  98.635
  ## 6 2009-06-17  98.940  98.960  98.945  98.660  98.670  98.670
  ## 7 2009-06-18  98.820  98.860  98.840  98.450  98.600  98.525
  ## 8 2009-06-19  98.935  98.965  98.935  98.640  98.680  98.630

  ed <- cbind(c(99.190,99.230,99.235,99.245,98.950,98.940,98.820,98.935),
              c(99.220,99.250,99.275,99.285,98.955,98.960,98.860,98.965),
              c(99.210,99.240,99.265,99.265,98.935,98.945,98.840,98.935),
              c(98.770,98.835,98.880,98.930,98.660,98.660,98.450,98.640),
              c(98.795,98.880,98.925,98.945,98.670,98.670,98.600,98.680),
              c(98.790,98.850,98.900,98.930,98.635,98.670,98.525,98.630))

  colnames(ed) <- c("ED2.BID","ED2.ASK","ED2.CLS","ED3.BID","ED3.ASK","ED3.CLS")

  ## The roll is at row 4

  roll.at <- c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)

  ## The pricemap is:
  
  ## Mark:      ED2.CLS
  ## Long:	ED2.ASK
  ## Short:	ED2.BID
  ## RollLong:  ED3.ASK
  ## RollShort: ED3.BID
  
  prcmap <- c(Mark="ED2.CLS",Long="ED2.ASK",Short="ED2.BID",RollLong="ED3.ASK",RollShort="ED3.BID")

  ## prices()
  
  ## Case #1: Long at roll time 
  checkEquals(prices(ed, 1, prcmap, roll.at)[4, "Price"], 99.245, checkNames=FALSE) ## Short price
  checkEquals(prices(ed, 1, prcmap, roll.at)[4, "Roll"],  98.945, checkNames=FALSE) ## RollLong price
  checkEquals(prices(ed, 1, prcmap, roll.at)[5, "Price"], 98.935, checkNames=FALSE) ## Mark price
  ## Case #2: Short at roll time 
  checkEquals(prices(ed, -1, prcmap, roll.at)[4, "Price"], 99.285, checkNames=FALSE) ## Long price
  checkEquals(prices(ed, -1, prcmap, roll.at)[4, "Roll"],  98.930, checkNames=FALSE) ## RollShort price
  checkEquals(prices(ed, -1, prcmap, roll.at)[5, "Price"], 98.935, checkNames=FALSE) ## Mark price
  ## Case #3: Flat at roll time 
  checkEquals(prices(ed, 0, prcmap, roll.at)[4, "Price"], 99.265, checkNames=FALSE) ## Mark price
  checkEquals(prices(ed, 0, prcmap, roll.at)[4, "Roll"],  98.945, checkNames=FALSE) ## RollLong price
  checkEquals(prices(ed, 0, prcmap, roll.at)[5, "Price"], 98.935, checkNames=FALSE) ## Mark price

  ## equity

  ## Equity at roll+1 (index 5)
  prc <- prices(ed, 1, prcmap, roll.at)
  checkEquals(equity(prc, states=1, roll.at=roll.at, percent=FALSE)[5, "Equity"], 1 + prc[5, 1] - (prc[1, 1] + prc[4, 2] - prc[4, 1]), checkNames=FALSE)

  ## trades
  ## trades(prices(ed, 1, prcmap, roll.at), states=1, roll.at=roll.at, percent=FALSE)
}
