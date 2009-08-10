prices <- function(x, pricemap=tsys(x)$pricemap, roll.at=FALSE){
  y <- as.matrix(x)[, pricemap, drop=FALSE]
  colnames(y) <- names(pricemap)
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
  h <- phasemap(states(x))
  y[which(h == "EL"), "Price"] <- y[which(h == "EL"), "Long"]
  y[which(h == "ES"), "Price"] <- y[which(h == "ES"), "Short"]
  y[which(h == "XL"), "Price"] <- y[which(h == "XL"), "Short"]
  y[which(h == "XS"), "Price"] <- y[which(h == "XS"), "Long"]
  RollAt <- which(as.logical(cbind(roll.at, y[, "Price"])[, 1]))
  y[which(RollAt & states(x) == -1), "Roll"] <- y[which(RollAt & states(x) == -1), "RollShort"]
  zoo(y, order.by=index(x))
}
