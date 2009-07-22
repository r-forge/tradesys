pricecols <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  attr(x, "tsts")$pricecols
}

"pricecols<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!is.list(value))
    value <- list(valuation=value[1], enterlong=value[1], entershort=value[1], exitlong=value[1],
                  exitshort=value[1], rolllong=value[1], rollshort=value[1])
  names(value) <- tolower(names(value))
  l <- list(valuation=1, enterlong=1, entershort=1, exitlong=1, exitshort=1, rolllong=1, rollshort=1)
  n <- which(names(value) %in% c("valuation","enterlong","entershort","exitlong","exitshort","rolllong","rollshort"))
  if(length(n) != 0)
    l <- replace(l, match(names(value[n]), names(l)), value[n])
  if("enter" %in% names(value))
    l <- replace(l, which(names(l) %in% c("enterlong","entershort")), value$enter)
  if("exit" %in% names(value))
    l <- replace(l, which(names(l) %in% c("exitlong","exitshort")), value$exit)
  if("long" %in% names(value))
    l <- replace(l, which(names(l) %in% c("enterlong","exitlong")), value$long)
  if("short" %in% names(value))
    l <- replace(l, which(names(l) %in% c("entershort","exitshort")), value$short)
  if("roll" %in% names(value))
    l <- replace(l, which(names(l) %in% c("rolllong","rollshort")), value$roll)
  lapply(l, function(y, x){
    if(y == which(colnames(x) == "St"))
      stop("pricecol cannot map to the 'St' column.")
    if(is.character(y))
      if(!y %in% colnames(x))
        stop(paste("pricecol", y, "is not in colnames(x)"))
    if(is.numeric(y))
      if(y > ncol(x))
        stop("a pricecol index exceeds ncol(x)")
  }, x)
  value <- l
  ## valuation column cannot have NA's. 
  if(any(is.na(x[, l$valuation])))
    stop(paste("NA's are not allowed in the valuation price column", pricecols$valuation))
  ##   ## ...other price cols with NA's get assigned valuation price
  ##   for(col in l){
  ##     if(any(n <- is.na(x[, col])))
  ##       x[which(n), col] <- x[which(n), l$valuation]
  ##   }
  attr(x, "tsts")$pricecols <- value
  x
}
