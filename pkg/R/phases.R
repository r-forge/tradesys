phases <- function(x){
  phasemap(states(x))
}

"phases<-" <- function(x, value){
  states(x) <- statemap(value)
  x
}
