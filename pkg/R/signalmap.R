signalmap <- function(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, entrywins=FALSE){
  entrywins <- entrywins[1]
  el <- cbind(el, es, xl, xs)[, 1]
  es <- cbind(el, es, xl, xs)[, 2]
  xl <- cbind(el, es, xl, xs)[, 3]
  xs <- cbind(el, es, xl, xs)[, 4]
  ## NA's are treated as FALSE
  el[is.na(el)] <- FALSE
  es[is.na(es)] <- FALSE
  xl[is.na(xl)] <- FALSE
  xs[is.na(xs)] <- FALSE
  ## Convert binary to integer
  x <- bin2int(cbind(el,es,xl,xs))
  ## Reduce the sixteen possible forms to six (0,1,2,3,4,8):
  x[x == 6] <- 4    ## 0110 [6]  --> 0100 [4]
  x[x == 9] <- 8    ## 1001 [9]  --> 1000 [8]
  x[x == 12] <- 0   ## 1100 [12] --> 0000 [0]
  x[x == 13] <- 1   ## 1101 [13] --> 0001 [1]
  x[x == 14] <- 2   ## 1110 [14] --> 0010 [2]
  x[x == 15] <- 3   ## 1111 [15] --> 0011 [3]
  if(entrywins){
    x[x == 5] <- 4  ## 0101 [5]  --> 0100 [4]
    x[x == 7] <- 4  ## 0111 [7]  --> 0100 [4]
    x[x == 10] <- 8 ## 1010 [10] --> 1000 [8]
    x[x == 11] <- 8 ## 1011 [11] --> 1000 [8]
  }else{
    x[x == 5] <- 1  ## 0101 [5]  --> 0001 [1]
    x[x == 7] <- 3  ## 0111 [7]  --> 0011 [3]
    x[x == 10] <- 2 ## 1010 [10] --> 0010 [2]
    x[x == 11] <- 3 ## 1011 [11] --> 0011 [3]
  }
  ## Signal to state map
  ## ---------------------------
  ## 0000 [0] --> s(t-1) 
  ## 0001 [1] --> max[s(t-1), 0] 
  ## 0010 [2] --> min[s(t-1), 0] 
  ## 0011 [3] --> 0
  ## 0100 [4] --> -1
  ## 1000 [8] --> 1
  y <- rep(NA, length(el))
  y[x == 3] <- 0
  y[x == 4] <- -1
  y[x == 8] <- 1
  if(is.na(y[1]))
    y[1] <- 0
  for(i in which(is.na(y))){
    if(x[i] == 0)
      y[i] <- y[i-1]
    if(x[i] == 1)
      y[i] <- max(y[i-1], 0)
    if(x[i] == 2)
      y[i] <- min(y[i-1], 0)
  }
  y
}
