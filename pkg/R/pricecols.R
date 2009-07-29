pricecols <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  attr(x, "tsts")$pricecols
}

"pricecols<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!is.list(value))
    l <- list(Mark=value[1], Long=value[1], Short=value[1], RollLong=value[1], RollShort=value[1])
  else
    l <- list(Mark=1, Long=1, Short=1, RollLong=NA, RollShort=NA)
  if(any(!names(value) %in% c("Mark","Long","Short","RollLong","RollShort")))
    stop("All names(value) must be among 'Mark','Long','Short','RollLong','RollShort'")
  l <- replace(l, match(names(value), names(l)), value)
  if("Roll" %in% names(value))
    l <- replace(l, which(names(l) %in% c("RollLong","RollShort")), value$Roll)
  if(is.na(l$RollLong))
    l$RollLong <- l$Long
  if(is.na(l$RollShort))
    l$RollShort <- l$Short
  lapply(l, function(y, x){
    if(is.character(y)){ 
      if(!y %in% colnames(x)) # must be one of these
        stop(paste("pricecol", y, "is not in colnames(x)"))
      if(tolower(y) %in% c("st","trade","size","roll","delta","equity")) # can't be one of these
        stop("No value may be among 'St','Trade','Size','Roll','Delta','Equity'")
    }
    if(is.numeric(y)){
      if(y > ncol(x))
        stop("a pricecol index exceeds ncol(x)")
      if(y %in% which(colnames(x) %in% c("St","Trade","Size","Roll","Delta","Equity"))) # can't be one of these
        stop("No value may index columns 'St','Trade','Size','Roll','Delta', or 'Equity'")
    }
  }, x)
##   ## valuation column cannot have NA's. 
##   if(any(is.na(x[, l$Mark])))
##     stop(paste("NA's are not allowed in the Mark price column", pricecols$Mark))
##   ## ...other price cols with NA's get assigned valuation price
##   for(col in l){
##     if(any(n <- is.na(x[, col])))
##       x[which(n), col] <- x[which(n), l$Mark]
##   }
  attr(x, "tsts")$pricecols <- l
  x
}
