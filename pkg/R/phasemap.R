phasemap <- function(states){
  if(!all(states %in% c(1,-1,0)))
    stop("states must be a numeric vector of 1s, -1s and 0s.")
  S1 <- states
  S0 <- c(0, states[-length(states)])
  x <- c()
  x[S1 == S0] <- 0            
  x[S1 ==  0 & S0 == -1] <- 1 
  x[S1 ==  0 & S0 ==  1] <- 2 
  x[S1 == -1 & S0 != -1] <- 4 
  x[S1 ==  1 & S0 !=  1] <- 8 
  x
}

