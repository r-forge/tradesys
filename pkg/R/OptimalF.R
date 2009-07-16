OptimalF <- function(payoffs, prob=NULL, by=.01){
  f <- seq(0, to=1, by=by)
  x <- GambleGrowth(payoffs, f, prob)
  f[which(x == max(x))]
}

GambleGrowth <- function(payoffs, f, prob=NULL){
  if(length(f) > 1)
    return(sapply(f, function(x) GambleGrowth(payoffs, x, prob)))
  if(is.null(prob))
    return(prod(1 + f * (-payoffs / min(payoffs)))^(1 / length(payoffs)))
  if(sum(prob) != 1)
    stop("sum(prob) must equal 1.")
  if(length(payoffs) != length(prob))
    stop("'payoffs' and 'prob' must be the same length.")
  prod(1 + f * (-payoffs / min(payoffs))^prob)
}

GambleGrowthBin <- function(odds, prob, f=Kelly(odds, prob)){
  (1 - f + f * (odds + 1))^prob * (1 - f)^(1 - prob) - 1
}


