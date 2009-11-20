test.splooth <- function(){

  m <- cbind(rnorm(100, sd=3) + 100, rnorm(100, sd=3) + 120, rnorm(100, sd=3) + 140)
  at <- runif(nrow(m)) < .2

  colnames(m) <- paste("c", 1:ncol(m), sep="") 
  rownames(m) <- paste("r", 1:nrow(m), sep="")

  cat("colnames(x) and rownames(x) are preserved in the return of splooth.. ")
  checkEquals(colnames(splooth(m, at), "diff"), colnames(m))
  checkEquals(rownames(splooth(m, at), "diff"), rownames(m))
  checkEquals(colnames(splooth(m, at), "ratio"), colnames(m))
  checkEquals(rownames(splooth(m, at), "ratio"), rownames(m))
  checkEquals(colnames(splooth(m, at), "wavg"), colnames(m))
  checkEquals(rownames(splooth(m, at), "wavg"), rownames(m))
  cat("OK.\n")

  cat("Last column of return is equal to last column of x.. ")
  checkEquals(splooth(m, at, "diff")[, ncol(m)], m[, ncol(m)])
  checkEquals(splooth(m, at, "ratio")[, ncol(m)], m[, ncol(m)])
  checkEquals(splooth(m, at, "wavg")[, ncol(m)], m[, ncol(m)])
  cat("OK.\n")

  ##
  ## Method: Difference ('diff')
  ##

  ## cat("Tests for splooth(x, at, method='diff')\n")

  ##
  ## Method: Ratio ('ratio')
  ##

  cat("Tests for splooth(x, at, method='ratio')\n")

  cat("Error raised if any values of x must are greater than zero.. ")
  mm <- m
  mm[7, 1] <- -100
  checkException(splooth(mm, at, "ratio"))
  checkException(splooth(mm, at, "ratio"))
  cat("OK.\n")

  ##
  ## Method: Weighted Average ('wavg')
  ##

  cat("Tests for splooth(x, at, method='wavg')\n")

  y <- splooth(m, at <- runif(nrow(m)) < .2, "wavg")

  cat("Y'[t,j] = Y[t,j+1] when t is a in which(at).. ")
  checkEquals(y[which(at), 1], m[which(at), 2])
  cat("OK.\n")

  cat("Y' is between Y[,j] and Y[,j+1].. ")
  checkTrue(all((y[, 1] > m[, 1])))
  checkTrue(all((y[, 1] <= m[, 2])))
  checkTrue(all((y[, 2] > m[, 2])))
  checkTrue(all((y[, 2] <= m[, 3])))
  cat("OK.\n")

}
