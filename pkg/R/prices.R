prices <- function(x, states=states(x)){
  if(is.tsts(x)){
    y <- as.matrix(x)[, unlist(lapply(pricecols(x), function(x, y) if(is.numeric(x)) colnames(y)[x] else x, x))]
    colnames(y) <- names(pricecols(x))
  }else{
    if(!is.matrix(x))
      x <- matrix(x, ncol=1, dimnames=list(NULL, "Mark"))
    if(!"Mark" %in% colnames(x))
      stop("'Mark' must be in colnames(x).")
    if(length(which(colnames(x) == "Mark")) > 1)
      stop("Only one colnames(x) may be 'Mark'.")
    y <- x[, match(c("Mark","Long","Short","RollLong","RollShort"), colnames(x), which(colnames(x) == "Mark"))]
    colnames(y) <- c("Mark","Long","Short","RollLong","RollShort")
  }
  y <- zoo(y, order.by=index(x))
  if(any(is.na(y[, "Mark"]))){ ## fill NA's in Mark column
    message("NA's in 'Mark' column.. filling with previous value")
    y[, "Mark"] <- na.locf(y[, "Mark"])
    y[, "Mark"] <- na.locf(y[, "Mark"], fromLast=TRUE)
  }
  for(i in colnames(y)){ ## ...other cols with NA's get Mark's value
    if(any(n <- is.na(y[, i]))){
      message(paste("NA's in", i, "column.. filling with value in 'Mark' column."))
      y[which(n), i] <- y[which(n), "Mark"]
    }
  }
  y <- cbind(Use=y[, "Mark"], y)
  h <- phasemap(states)
  y[which(h == "EL"), "Use"] <- y[which(h == "EL"), "Long"]
  y[which(h == "ES"), "Use"] <- y[which(h == "ES"), "Short"]
  y[which(h == "XL"), "Use"] <- y[which(h == "XL"), "Short"]
  y[which(h == "XS"), "Use"] <- y[which(h == "XS"), "Long"]
  y
  y
}
