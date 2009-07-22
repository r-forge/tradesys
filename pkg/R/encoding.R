addsig <- function(x, name, expr){
  if(!inherits(x, "tsts"))
    stop("x must be class 'tsts'.")
  if(!tolower(name) %in% c("el","es","xl","xs"))
    stop("name must be one of 'el', 'es', 'xl', or 'xs'.")
  y <- x
  attr(y, "tsts")$exprsigs[[tolower(name)]] <- substitute(expr)
  coredata(y) <- coredata(y)
  eval.parent(substitute(x <- y))
}

addcol <- function(x, name, expr){
  y <- x
  if(name == "St")
    stop("name cannot be 'St', the column name of the states vector")
  if(!inherits(y, "tsts"))
    stop("x must be class 'tsts'.")
  if(is.null(attr(y, "tsts")$exprcols)){
    attr(y, "tsts")$exprcols <- list(substitute(expr))
    names(attr(y, "tsts")$exprcols) <- name
  }else{
    attr(y, "tsts")$exprcols[[name]] <- substitute(expr)
  }
  coredata(y) <- coredata(y)
  eval.parent(substitute(x <- y))
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
