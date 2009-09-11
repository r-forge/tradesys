test.roll <- function(){

  ## These are the closing Bid/Ask/Close prices for the second and third Eurodollar
  ## series. The contract rolled on 06/15, so ED2 represents EDU9 until 06/16, at
  ## which time it starts representing EDZ9. (Similarly, ED3 represents EDZ9 before
  ## and EDH0 after.) 

  ##              ED2.BID ED2.ASK ED2.CLS ED3.CLS
  ## 1 2009-06-10  99.190  99.220  99.210  98.790
  ## 2 2009-06-11  99.230  99.250  99.240  98.850
  ## 3 2009-06-12  99.235  99.275  99.265  98.900
  ## 4 2009-06-15  99.245  99.285  99.265  98.930
  ## 5 2009-06-16  98.950  98.955  98.935  98.635
  ## 6 2009-06-17  98.940  98.960  98.945  98.670
  ## 7 2009-06-18  98.820  98.860  98.840  98.525
  ## 8 2009-06-19  98.935  98.965  98.935  98.630

  ed <- cbind(c(99.190,99.230,99.235,99.245,98.950,98.940,98.820,98.935),
              c(99.220,99.250,99.275,99.285,98.955,98.960,98.860,98.965),
              c(99.210,99.240,99.265,99.265,98.935,98.945,98.840,98.935),
              c(98.790,98.850,98.900,98.930,98.635,98.670,98.525,98.630))

  colnames(ed) <- c("ED2.BID","ED2.ASK","ED2.CLS","ED3.CLS")

  ## The roll is at row 4

  roll.at <- c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)

  ## The pricemap is:
  
  ## Mark:      ED2.CLS
  ## Long:	ED2.ASK
  ## Short:	ED2.BID
  ## RollOut:   ED3.CLS
  ## RollIn:    ED3.CLS
  
  prcmap <- c(Mark="ED2.CLS",Long="ED2.ASK",Short="ED2.BID",RollOut="ED2.CLS",RollIn="ED3.CLS")

  ## prices()
  
  checkEquals(prices(ed, 1, prcmap, roll.at)[4, "RollOut"], 99.265, checkNames=FALSE) ## ED2 Close
  checkEquals(prices(ed, 1, prcmap, roll.at)[4, "RollIn"],  98.930, checkNames=FALSE) ## ED3 Close
  checkEquals(prices(ed, 1, prcmap, roll.at)[5, "Price"],   98.935, checkNames=FALSE) ## Mark price

}
