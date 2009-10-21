## Labels each consecutive string of 1s or -1s with 1, 2, 3, ..., etc. 0s get 0.
## TradeID(c(0,1,1,1,-1,-1,0,0,1,1))
## TradeID(c(0,0,0,0,0,0,0,0,0,0))
## TradeID(c(1,1,1,1,1,0,1,1,1,1))

TradeID <- function(states){
  cumsum(as.numeric(as.logical(abs(c(states[1], diff(states)))) & abs(states))) * abs(states)
}

TradeEntries <- function(states){ ## Entry at i when s(i) != s(i-1) and s(i) != 0
  as.logical(as.logical(c(states[1], diff(states))) * states)
}

TradeExits <- function(states){   ## Exit at i when s(i) != s(i-1) and s(i-1) != 0
  c(FALSE, as.logical(diff(states) * states[-length(states)]))
}

which.expand <- function(x, leading=1){
  x[which(x)] <- which(x)
  x[which(!x)] <- NA
  if(is.na(x[1]))
    x[1] <- leading
  na.locf(x, FALSE)
}

## c(1,1,3,3,5,5,5)
#which.expand(c(FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE))
## c(1,1,1,1,1,1,1)
#which.expand(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE))
## c(1,2,3,4,5,6,7)
#which.expand(c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE))
