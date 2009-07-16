MA <- function(x, width){
  rollmean(x, width, align="right", na.pad=TRUE)
}

HIGH <- function(x, width){
  rollmax(x, width, align="right", na.pad=TRUE)
}

LOW <- function(x, width){
  rollapply(x, width, min, align="right", na.pad=TRUE)
}
