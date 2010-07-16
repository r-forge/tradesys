tsr <- function(prices, states=1, delta=1/prices){
  if(length(prices) %% length(delta) != 0){
    stop("length(delta) must be a multiple of length(prices)")
  }else{
    delta <- cbind(delta, prices)[,1]
  }
  if(length(prices) %% length(states) != 0){
    stop("length(states) must be a multiple of length(prices)")
  }else{
    states <- cbind(states, prices)[,1]
  }
  c(0, diff(prices) * states[-length(states)] * delta[-length(delta)])
}
