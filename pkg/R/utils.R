pricemapper <- function(pricemap){
  if(!is.vector(pricemap) | !is.character(pricemap))
    stop("pricemap must be a character vector of column names.")
  if(length(pricemap) > 5)
    stop("length(pricemap) must be <= 5")
  if(is.null(names(pricemap))) ## .. then order determines mapping when no names
    names(pricemap) <- c("Mark","Long","Short","RollLong","RollShort")[1:length(pricemap)]
  if(any(duplicated(names(pricemap))))
    stop("names(pricemap) must be unique")
  if(any(!names(pricemap) %in% c("Mark","Long","Short","RollLong","RollShort")))
    stop("All names(pricemap) must be among 'Mark','Long','Short','RollLong','RollShort'")
  x <- c(pricemap["Mark"], pricemap["Long"], pricemap["Short"], pricemap["RollLong"], pricemap["RollShort"])
  names(x) <- c("Mark","Long","Short","RollLong","RollShort")
  if(is.na(x["Mark"]))
    stop("pricemap must be passed a value for 'Mark'")
  if(is.na(x["Long"]))
    x["Long"] <- x["Mark"]
  if(is.na(x["Short"]))
    x["Short"] <- x["Mark"]
  if(is.na(x["RollShort"]))
    x["RollShort"] <- x["Short"]
  if(is.na(x["RollLong"]))
    x["RollLong"] <- x["Long"]
  x
}

## Labels each consecutive string of 1s or -1s with 1, 2, 3, ..., etc. 0s get 0.
## TradeID(c(0,1,1,1,-1,-1,0,0,1,1))
## TradeID(c(0,0,0,0,0,0,0,0,0,0))
## TradeID(c(1,1,1,1,1,0,1,1,1,1))

TradeID <- function(states){
  cumsum(as.numeric(as.logical(abs(c(states[1], diff(states)))) & abs(states))) * abs(states)
}

## TradeTable(c(0,1,1,1,-1,-1,0,0,1,1))
## TradeTable(c(0,0,0,0,0,0,0,0,0,0))
## TradeTable(c(1,1,1,1,1,0,1,1,1,1))

TradeTable <- function(states){
  if(sum(states) == 0)
    return(NULL)
  ID <- TradeID(states)
  Tr <- unique(ID[ID != 0])
  Ei <- sapply(Tr, function(x) min(which(x == ID)))
  Xi <- sapply(Tr, function(x) max(which(x == ID)) + 1)
  cbind(ID=Tr, LS=states[Ei], Ei, Xi)
}
