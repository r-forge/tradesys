test.splice <- function(){

  ## nrow(x) must be a multiple of length(at)
  checkException(splice(1:10, c(T,T,T)))

  ## ncol of return equals max(length(which(at)) - ncol(x) + 1, 1)
  checkEquals(ncol(splice(matrix(1:10, ncol=1), rep(T, 10))), 10)
  checkEquals(ncol(splice(matrix(1:20, ncol=2), rep(T, 10))), 9)
  checkEquals(ncol(splice(matrix(1:30, ncol=3), rep(T, 10))), 8)
  checkEquals(ncol(splice(matrix(1:30, ncol=3), c(T,F,F,F,F,F,F,F,F,F))), 1) # which(at) = 1
  checkEquals(ncol(splice(matrix(1:30, ncol=3), c(T,F,F,F,F,T,F,F,F,F))), 1) # which(at) = 2
  checkEquals(ncol(splice(matrix(1:30, ncol=3), c(T,F,F,F,F,T,F,T,F,F))), 1) # which(at) = 3
  checkEquals(ncol(splice(matrix(1:30, ncol=3), c(T,F,T,F,F,T,F,T,F,F))), 2) # which(at) = 4

  ## By convention, return x if any(at) is FALSE
  checkEquals(splice(matrix(1:30, ncol=3), F), matrix(1:30, ncol=3))

  ##
  ## Test all values using this diagonal function
  ##

  diag.test <- function(r, j, at, x){ 
    if(length(r) > 1)
      return(sapply(r, diag.test, j, at, x))
    at <- as.logical(cbind(at, 1:nrow(x))[, 1])
    x[r, sum(at[which(at)[j]:r])]
  }

  m <- matrix(1:100, ncol=5)

  checkEquals(splice(m, c(T,F))[1:10, 1], diag.test(1:10, 1, c(T,F), m))
  checkEquals(splice(m, c(T,F))[3:12, 2], diag.test(3:12, 2, c(T,F), m))
  checkEquals(splice(m, c(T,F))[5:14, 3], diag.test(5:14, 3, c(T,F), m))
  checkEquals(splice(m, c(T,F))[7:16, 4], diag.test(7:16, 4, c(T,F), m))
  checkEquals(splice(m, c(T,F))[9:18, 5], diag.test(9:18, 5, c(T,F), m))
  checkEquals(splice(m, c(T,F))[11:20, 6], diag.test(11:20, 6, c(T,F), m))

  cat("Test 100 random matrix with random splice times.\n")
  for(j in 1:100){
    N <- round(runif(1, 1, 10))
    m <- matrix(rnorm(100 * N), ncol=N)          
    at <- rep(F, nrow(m))
    at[sample(1:length(at), round(runif(1, 1, 20)))] <- T
    x <- splice(m, at)
    n <- max(length(which(at)) - ncol(m) + 1, 1)
    cat("nrow(x) =",          format(nrow(x), width=3),
        "ncol(x)=",           format(ncol(x), width=3),
        "length(which(at))=", format(length(which(at)), width=3), "\n")
    for(i in 1:n){
      if(i == n)
        rows <- which(at)[i]:nrow(m)
      else
        rows <- which(at)[i]:(which(at)[i + ncol(m)] - 1)
      checkEquals(x[rows, i], diag.test(rows, i, at, m))
    }
  }

}
