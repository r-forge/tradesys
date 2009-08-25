prices <- function(x, states, pricemap, roll.at=FALSE){
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
  ## process roll.at
  roll.at <- as.logical(cbind(roll.at, x)[, 1])
  if(any(is.na(y[, "Mark"]))){ ## fill NA's in Mark column
    message("NA's in 'Mark' column.. filling with previous value")
    y[, "Mark"] <- na.locf(y[, "Mark"])
    y[, "Mark"] <- na.locf(y[, "Mark"], fromLast=TRUE)
  }
  for(i in colnames(y)){ ## ...other cols with NA's get Mark's value
    if(any(n <- is.na(y[, i]))){
      message(paste("NA's in", i, "column.. filling with value in 'Mark' column."))
      y[n, i] <- y[n, "Mark"]
    }
  }
  y <- cbind(Price=y[, "Mark"], RollPrice=y[, "RollLong"], y)
  h <- phasemap(states)
  y[h == "EL", "Price"] <- y[h == "EL", "Long"]
  y[h == "ES", "Price"] <- y[h == "ES", "Short"]
  y[h == "XL", "Price"] <- y[h == "XL", "Short"]
  y[h == "XS", "Price"] <- y[h == "XS", "Long"]
  y[roll.at & states == 1, "Price"] <-   y[roll.at & states == 1, "Short"]
  y[roll.at & states == -1, "Price"] <-   y[roll.at & states == -1, "Long"] 
  y[states == -1, "RollPrice"] <- y[states == -1, "RollShort"]
  y
}
