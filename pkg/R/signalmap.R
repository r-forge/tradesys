signalmap <- function(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, entrywins=FALSE, retval=c("states","phases","binary")){
  entrywins <- entrywins[1]
  retval <- retval[1]
  m <- cbind(el, es, xl, xs)
  el <- m[, 1]
  es <- m[, 2]
  xl <- m[, 3]
  xs <- m[, 4]
  ## NA's are treated as FALSE
  el[is.na(el)] <- FALSE
  es[is.na(es)] <- FALSE
  xl[is.na(xl)] <- FALSE
  xs[is.na(xs)] <- FALSE
  ## Convert binary to integer
  x <- 8 * el + 4 * es + 2 * xl + xs
  if(retval == "binary")
    return(x)
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
  if(retval=="states")
    x <- statemap(x)
  x
}
