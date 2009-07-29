state.changes <- function(states){
  as.logical(c(abs(states[1]), sapply(abs(diff(states)), min, 1) == 1))
}
