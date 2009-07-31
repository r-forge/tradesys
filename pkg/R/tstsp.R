tstsp <- function(x){
  if(!is.tsts(x))
    return(invisible(NULL))
  attr(x, "tstsp")
}

"tstsp<-" <- function(x, value){
  attr(x, "tstsp") <- value
  x
}
