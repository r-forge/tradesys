statechg <- function(states){
  if(!is.numeric(states)|any(!states %in% c(1,0,-1)))
    stop("states must be a numeric vector of 1s, 0s, and -1s.")
  as.logical(c(abs(states[1]), sapply(abs(diff(states)), min, 1) == 1))
}
