pricemapper <- function(pricemap){
  if(!is.vector(pricemap) | !is.character(pricemap))
    stop("pricemap must be a character vector of column names.")
  if("roll" %in% tolower(names(pricemap))){
    RollN <- which(tolower(names(pricemap)) == "roll")[1]
    pricemap["RollOut"] <- pricemap[RollN]
    pricemap["RollIn"] <- pricemap[RollN]
    pricemap <- pricemap[-RollN]
  }
  if(length(pricemap) > 5)
    stop("length(pricemap) must be <= 5")
  if(is.null(names(pricemap))) ## .. then order determines mapping when no names
    names(pricemap) <- c("Mark","Long","Short","RollOut","RollIn")[1:length(pricemap)]
  if(any(duplicated(names(pricemap))))
    stop("names(pricemap) must be unique")
  if(any(!names(pricemap) %in% c("Mark","Long","Short","RollOut","RollIn")))
    stop("All names(pricemap) must be among 'Mark','Long','Short','RollOut','RollIn'")
  x <- c(pricemap["Mark"], pricemap["Long"], pricemap["Short"], pricemap["RollOut"], pricemap["RollIn"])
  names(x) <- c("Mark","Long","Short","RollOut","RollIn")
  if(is.na(x["Mark"]))
    stop("pricemap must be passed a value for 'Mark'")
  if(any(is.na(x)))
    x[is.na(x)] <- x["Mark"]
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
  if(all(states == 0))
    return(NULL)
  ID <- TradeID(states)
  Tr <- unique(ID[ID != 0])
  Ei <- sapply(Tr, function(x) min(which(x == ID)))
  Xi <- sapply(Tr, function(x) max(which(x == ID)) + 1)
  cbind(ID=Tr, LS=states[Ei], Ei, Xi)
}

TradeEntries <- function(states){ ## Entry at i when s(i) != s(i-1) and s(i) != 0
  as.logical(as.logical(c(states[1], diff(states))) * states)
}

TradeExits <- function(states){   ## Exit at i when s(i) != s(i-1) and s(i-1) != 0
  c(FALSE, as.logical(diff(states) * states[-length(states)]))
}

## x is a boolean vector. The return is a numeric vector v of the same
## length, where v[i] is the offset of first TRUE value in x after i.
## true.right(c(FALSE,TRUE, TRUE,FALSE, TRUE,FALSE)) ## 2,3,5,5,NA,NA
## true.right(c(TRUE, TRUE, TRUE,FALSE, TRUE,FALSE)) ## 2,3,5,5,NA,NA
## true.right(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)) ## 2,3,4,5,6,NA
## true.right(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE))## NA,NA,NA,NA,NA,NA

true.right <- function(x, missing=NA){
  sapply(1:length(x), function(y){
    if(any(which(x) > y))
      return(min(which(x)[which(x) > y]))
    missing
  })
}


