states <- function(x){
  if(!"St" %in% colnames(x))
    return(FALSE)
  y <- x[, "St"]
  names(y) <- NULL
  y
}
