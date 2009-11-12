changemap <- function(states){
  round(c(states[1], diff(states)) / 1.5)
}
