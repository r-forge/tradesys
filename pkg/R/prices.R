prices <- function(x, states, pricemap){
  ## process x
  if(is.tsts(x)){
    states <- x[, "St"]
    pricemap <- tsys(x)$pricemap
  }
  x <- as.matrix(x)
  if(any(!pricemap %in% colnames(x)))
    stop("all pricemap must be in colnames(x)")
  pricemap <- pricemapper(pricemap)
  y <- x[, pricemap, drop=FALSE]
  colnames(y) <- names(pricemap)
  ## process states
  if(nrow(x) %% length(states) != 0)
    stop("length(states) must be a multiple of nrow(x)")
  states <- cbind(states, x)[, 1]
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
  y <- cbind(Price=y[, "Mark"], Roll=y[, "RollLong"], y)
  h <- phasemap(states)
  y[which(h == "EL"), "Price"] <- y[which(h == "EL"), "Long"]
  y[which(h == "ES"), "Price"] <- y[which(h == "ES"), "Short"]
  y[which(h == "XL"), "Price"] <- y[which(h == "XL"), "Short"]
  y[which(h == "XS"), "Price"] <- y[which(h == "XS"), "Long"]
  y[which(states == -1), "Roll"] <- y[which(states == -1), "RollShort"]
  y
}
