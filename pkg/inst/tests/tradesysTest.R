test.tradesys <- function(){
  x <- tradesys(c("Bid","Ask","Close"))
  x$el <- quote(SMA(Close, 60) >= SMA(Close, 120))
  x$es <- quote(SMA(Close, 60) < SMA(Close, 120))
  x$pricemap <- c(Mark="Close", Long="Ask", Short="Bid")
  x$entrywins <- TRUE
  x$entrycond <- TRUE
  checkEquals(x, tradesys(c("Bid","Ask","Close"),
                          pricemap=c(Mark="Close", Long="Ask", Short="Bid"),
                          el=SMA(Close, 60) >= SMA(Close, 120),
                          es=SMA(Close, 60) < SMA(Close, 120),
                          entrywins=TRUE, entrycond=TRUE))
  y <- as.list(x)
  checkException(tradesys(c("Bid","Ask","Close"), entrywins = "Bam!"), silent=TRUE)          ## Must be logical
  checkException(tradesys(c("Bid","Ask","Close"), pricemap = "Bam!"), silent=TRUE)           ## Name not in datavars
  checkException(tradesys(c("Bid","Ask","Close"), pricemap = c(Mark="Open")), silent=TRUE)   ## Name not in datavars
  checkException(tradesys(c("Bid","Ask","Close"), pricemap = c(Marc="Close")), silent=TRUE)  ## Invalid label
  checkException(tradesys(c("Bid","Ask","Close"), pricemap = c(Long="Close", Short="Close")), silent=TRUE)  ## Nothing passed to Mark
  checkException(tradesys(c("Bid","Ask","Close"), pricemap = c(Mark="Close", Mark="Close")), silent=TRUE)   ## Duplicated names
}
