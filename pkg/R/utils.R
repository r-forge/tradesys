statechg <- function(states){
  if(!is.numeric(states)|any(!states %in% c(1,0,-1)))
    stop("states must be a numeric vector of 1s, 0s, and -1s.")
  as.logical(c(abs(states[1]), sapply(abs(diff(states)), min, 1) == 1))
}


processPriceCols <- function(x, pclist){ ## x is a tsts object, pclist is a list of pricecol names
  if(!is.list(pclist))
    l <- list(Mark=pclist[1], Long=pclist[1], Short=pclist[1], RollLong=pclist[1], RollShort=pclist[1])
  else
    l <- list(Mark=1, Long=1, Short=1, RollLong=NA, RollShort=NA)
  if(any(!names(pclist) %in% c("Mark","Long","Short","RollLong","RollShort")))
    stop("All names(pclist) must be among 'Mark','Long','Short','RollLong','RollShort'")
  l <- replace(l, match(names(pclist), names(l)), pclist)
  if("Roll" %in% names(pclist))
    l <- replace(l, which(names(l) %in% c("RollLong","RollShort")), pclist$Roll)
  if(is.na(l$RollLong))
    l$RollLong <- l$Long
  if(is.na(l$RollShort))
    l$RollShort <- l$Short
  lapply(l, function(y, x){
    if(is.character(y)){ 
      if(!y %in% colnames(x)) # must be one of these
        stop(paste("pricecol", y, "is not in colnames(x)"))
      if(tolower(y) %in% c("st","trade","size","roll","delta","equity")) # can't be one of these
        stop("No pclist may be among 'St','Trade','Size','Roll','Delta','Equity'")
    }
    if(is.numeric(y)){
      if(y > ncol(x))
        stop("a pricecol index exceeds ncol(x)")
      if(y %in% which(colnames(x) %in% c("St","Trade","Size","Roll","Delta","Equity"))) # can't be one of these
        stop("No pclist may index columns 'St','Trade','Size','Roll','Delta', or 'Equity'")
    }
  }, x)
  l
}
