splice <- function(x, at){
  if(is.zoo(x))
    return(zoo(splice(coredata(x), at), order.by=index(x)))
  x <- as.matrix(x)
  at <- as.logical(cbind(at, 1:nrow(x))[, 1])
  w <- which(at)
  n <- max(length(w) - ncol(x) + 1, 1) 
  y <- matrix(NA, nrow=nrow(x), ncol=n)
  for(j in 1:n){
    if(n == 1)
      rows <- seq(w[j], w[length(w)] - 1)
    else if(n == j)
      rows <- seq(w[j], nrow(x))
    else
      rows <- seq(w[j], w[j + ncol(x)] - 1)
    cols <- cumsum(at[rows])
    y[rows, j] <- sapply(1:length(rows), function(i){x[rows[i], cols[i]]})
  }
  y
}
