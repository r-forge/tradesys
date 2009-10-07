prices <- function(x, states, pricemap, roll.at=FALSE){
  ## process x
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
    y[, "Mark"] <- na.locf(y[, "Mark"])
    y[, "Mark"] <- na.locf(y[, "Mark"], fromLast=TRUE)
  }
  for(i in colnames(y)){ ## ...other cols with NA's get Mark's value
    if(any(n <- is.na(y[, i]))){
      y[n, i] <- y[n, "Mark"]
    }
  }
  y <- cbind(Price=y[, "Mark"], y)
  h <- phasemap(states)
  y[h == 8, "Price"] <- y[h == 8, "Long"]
  y[h == 4, "Price"] <- y[h == 4, "Short"]
  y[h == 2, "Price"] <- y[h == 2, "Short"]
  y[h == 1, "Price"] <- y[h == 1, "Long"]
  y
}
