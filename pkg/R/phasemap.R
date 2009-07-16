phasemap <- function(states){
  if(is.tsts(states))
    states <- states[, "states"]
  if(!all(states %in% c(1,-1,0)))
    stop("states must be a numeric vector of 1s, -1s and 0s.")
  x <- rep("UC", length(states))
  n <- which(c(states[1], diff(states)) != 0) # state changes
  for(i in n){
     if(i == 1){ ## if states[1] is long or short
      if(states[1] == 1)
        x[1] <- "EL"
      if(states[1] == -1)
        x[1] <- "ES"
      
    }else{
      if(states[i] == 1 & states[i-1] != 1) 
        x[i] <- "EL" ## enter long
      else if(states[i] == -1 & states[i-1] != -1)
        x[i] <- "ES" ## enter short
      else if(states[i] == 0 & states[i-1] == 1)
        x[i] <- "XL" ## exit long
      else if(states[i] == 0 & states[i-1] == -1)
        x[i] <- "XS" ## exit short
      else
        stop("logical error!")
    }
  }
  x
}

