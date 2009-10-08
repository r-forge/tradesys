statemap <- function(phases){
  if(any(!phases %in% c(0,1,2,3,4,8)))
    stop("phases must be a vector consisting of 0, 1, 2, 3, 4, and 8")

  ## 0000 [0] --> s(t) = s(t-1) 
  ## 0001 [1] --> s(t) = max[s(t-1), 0] 
  ## 0010 [2] --> s(t) = min[s(t-1), 0] 
  ## 0011 [3] --> s(t) = 0
  ## 0100 [4] --> s(t) = -1
  ## 1000 [8] --> s(t) = 1

  x <- c()
  x[which(phases == 8)] <- 1
  x[which(phases == 4)] <- -1
  x[which(phases == 3)] <- 0
  if(is.na(x[1]))
    x[1] <- 0
  for(i in (which(phases[-1] %in% 0:2) + 1)) ## ignore phases[1]
    x[i] <- switch(phases[i] + 1, x[i-1], max(0, x[i-1]), min(0, x[i-1]))
  x
}
