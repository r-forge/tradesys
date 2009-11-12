test.statemap <- function(){
  ## statemap must be invertable
  set.seed(399)
  for(i in 1:1000) ## Test with 1000, 100-length random state vectors
    checkEquals(statemap(phasemap(St <- round(runif(100, -1, 1)))), St)

  ## Check all 36 permutations an h(i), h(i-1) pair
  checkEquals(statemap(c(0,0)), c(0,0))
  checkEquals(statemap(c(0,1)), c(0,0))
  checkEquals(statemap(c(0,2)), c(0,0))
  checkEquals(statemap(c(0,3)), c(0,0))
  checkEquals(statemap(c(0,4)), c(0,-1))
  checkEquals(statemap(c(0,8)), c(0,1))
  checkEquals(statemap(c(1,0)), c(0,0))
  checkEquals(statemap(c(1,1)), c(0,0))
  checkEquals(statemap(c(1,2)), c(0,0))
  checkEquals(statemap(c(1,3)), c(0,0))
  checkEquals(statemap(c(1,4)), c(0,-1))
  checkEquals(statemap(c(1,8)), c(0,1))
  checkEquals(statemap(c(2,0)), c(0,0))
  checkEquals(statemap(c(2,1)), c(0,0))
  checkEquals(statemap(c(2,2)), c(0,0))
  checkEquals(statemap(c(2,3)), c(0,0))
  checkEquals(statemap(c(2,4)), c(0,-1))
  checkEquals(statemap(c(2,8)), c(0,1))
  checkEquals(statemap(c(3,0)), c(0,0))
  checkEquals(statemap(c(3,1)), c(0,0))
  checkEquals(statemap(c(3,2)), c(0,0))
  checkEquals(statemap(c(3,3)), c(0,0))
  checkEquals(statemap(c(3,4)), c(0,-1))
  checkEquals(statemap(c(3,8)), c(0,1))
  checkEquals(statemap(c(4,0)), c(-1,-1))
  checkEquals(statemap(c(4,1)), c(-1,0))
  checkEquals(statemap(c(4,2)), c(-1,-1))
  checkEquals(statemap(c(4,3)), c(-1,0))
  checkEquals(statemap(c(4,4)), c(-1,-1))
  checkEquals(statemap(c(4,8)), c(-1,1))
  checkEquals(statemap(c(8,0)), c(1,1))
  checkEquals(statemap(c(8,1)), c(1,1))
  checkEquals(statemap(c(8,2)), c(1,0))
  checkEquals(statemap(c(8,3)), c(1,0))
  checkEquals(statemap(c(8,4)), c(1,-1))
  checkEquals(statemap(c(8,8)), c(1,1))

  ## changemap in terms of phasemap
  s <- sample(c(1,0,-1), 1000, TRUE)
  checkEquals(changemap(s), sapply(phasemap(s), function(x){switch(x+1, 0, 1, -1, , -1, , , , 1)}))

}
