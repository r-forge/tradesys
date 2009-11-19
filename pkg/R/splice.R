splice <- function(x, at){
  if(is.zoo(x))
    return(zoo(splice(coredata(x), at), order.by=index(x)))
  x <- as.matrix(x)
  if((nrow(x) %% length(at)) != 0)
    stop("length(at) must be a multiple of nrow(x)")
  at <- as.logical(try(cbind(at, 1:nrow(x))[, 1]))
  if(!any(at))
    return(x)
  w <- which(at)
  n <- max(length(w) - ncol(x) + 1, 1) 
  y <- matrix(NA, nrow=nrow(x), ncol=n)
  for(j in 1:n){
    if(n == j)
      rows <- seq(w[j], nrow(x))
    else
      rows <- seq(w[j], w[j + ncol(x)] - 1)
    cols <- cumsum(at[rows])
    y[rows, j] <- sapply(1:length(rows), function(i){x[rows[i], cols[i]]})
  }
  y
}
