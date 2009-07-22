addsig <- function(x, name, expr){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!tolower(name) %in% c("el","es","xl","xs"))
    stop("name must be one of 'el', 'es', 'xl', or 'xs'.")
  y <- x
  attr(y, "tsts")$signals[[tolower(name)]] <- substitute(expr)
  coredata(y) <- coredata(y)
  eval.parent(substitute(x <- y))
}

addcol <- function(x, name, expr){
  y <- x
  if(name == "St")
    stop("name cannot be 'St', that is the column name of the states vector")
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(is.null(attr(y, "tsts")$columns)){
    attr(y, "tsts")$columns <- list(substitute(expr))
    names(attr(y, "tsts")$columns) <- name
  }else{
    attr(y, "tsts")$columns[[name]] <- substitute(expr)
  }
  coredata(y) <- coredata(y)
  eval.parent(substitute(x <- y))
}

getsig <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  attr(x, "tsts")$signals
}

getcol <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  attr(x, "tsts")$columns
}

entrywins <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  attr(x, "tsts")$entrywins
}

"entrywins<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!is.logical(value))
    stop("value must be logical.")
  attr(x, "tsts")$entrywins <- value
  coredata(x) <- coredata(x)
  x
}

entrycond <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  attr(x, "tsts")$entrycond
}

"entrycond<-" <- function(x, value){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  if(!is.logical(value))
    stop("value must be logical.")
  attr(x, "tsts")$entrycond <- value
  coredata(x) <- coredata(x)
  x
}
