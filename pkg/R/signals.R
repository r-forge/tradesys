signals <- function(x){
  if(!"signals" %in% names(attributes(x)))
    return(NULL)
  l <- lapply(attr(x, "signals"), eval, envir=as.list(as.data.frame(as.matrix(x))))
  do.call("cbind", l)
}

"signals<-" <- function(x, type="el", value){
  type <- tolower(type[1])
  if(!type %in% c("el","es","xl","xs"))
    stop("type must be one of 'el', 'es', 'xl', 'xs'.")
  if(!"signals" %in% names(attributes(x)))
    attr(x, "signals") <- list(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE)
  attr(x, "signals")[[type]] <- value
  x
}
