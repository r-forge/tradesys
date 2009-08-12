tradesys <- function(datavars, pricemap=c(Mark=datavars[1]), el=FALSE, es=FALSE, 
                     xl=FALSE, xs=FALSE, delta=1, size.at=as.logical(c(states[1], diff(states))),
                     roll.at=FALSE, percent=TRUE, entrywins=FALSE,
                     entrycond=FALSE, exprvars=NULL){
  l <- list()
  ## Process datavars
  if(any(duplicated(datavars)))
    stop("datavars be unique.")
  l$datavars <- datavars
  ## Process pricemap arg
  if(any(!pricemap %in% c(datavars, names(exprvars))))
    stop("all pricemap must be in datavars or names(exprvars)")
  l$pricemap <- pricemapper(pricemap)
  ## Process formulae arg (we expect a named list of calls)
  if(!is.null(exprvars)){
    if(any(unlist(lapply(exprvars, class)) != "call"))
      stop("exprvars must be a list of call objects")
    if(any(names(exprvars) == ""))
      stop("all exprvars must be named")
    if(any(duplicated(names(exprvars))))
      stop("names(exprvars) be unique")
    if(any(names(exprvars) %in% datavars))
      stop("exprvars cannot have a name matching datavars.")
    l$exprvars <- exprvars
  }else{
    l["exprvars"] <- list(NULL)
  }
  ## Process expression args
  l$el <- substitute(el)
  l$es <- substitute(es)
  l$xl <- substitute(xl)
  l$xs <- substitute(xs)
  l$delta <- substitute(delta)
  l$size.at <- substitute(size.at)
  l$roll.at <- substitute(roll.at)
  ## Process logical args
  if(!is.logical(percent))
    stop("percent must be logical")
  if(!is.logical(entrywins))
    stop("entrywins must be logical")
  if(!is.logical(entrycond))
    stop("entrycond must be logical")
  l$percent <- percent
  l$entrywins <- entrywins
  l$entrycond <- entrycond
  class(l) <- "tsys"
  l
}

##
## Methods
##

print.tsys <- function(x, ...){
  cat("datavars: ", x$datavars, "\n")
  cat("pricemap:", paste(names(x$pricemap), x$pricemap, sep="="), "\n")
  cat("el:", format(x$el), "\n")
  cat("es:", format(x$es), "\n")
  cat("xl:", format(x$xl), "\n")
  cat("xs:", format(x$xs), "\n")
  if(!is.null(x$exprvars)){
    cat("** exprvars **\n")
    for(i in 1:length(x$exprvars))
      cat(names(x$exprvars)[i],":", format(x$exprvars[[i]]), "\n")
  }
  cat("delta:", format(x$delta), "\n")
  cat("size.at:", format(x$size.at), "\n")
  cat("roll.at:", format(x$roll.at), "\n")
  cat("percent:", format(x$percent), "\n")
  cat("entrywins:", format(x$entrywins), "\n")
  cat("entrycond:", format(x$entrycond), "\n")
}

"$<-.tsys" <- function(x, i=NULL, value){
  y <- as.list.tsys(x)
  if(is.null(i))
    y <- value
  else
    y[[i]] <- value
  y <- try(do.call("tradesys", y))
  if(class(y) == "try-error")
    return(x)
  y
}

"[[<-.tsys" <- function(x, i, value){
  "$<-.tsys"(x, i, value)
}

"[<-.tsys" <- function(x, i, value){
  y <- as.list.tsys(x)
  y[i] <- value
  y <- try(do.call("tradesys", y))
  if(class(y) == "try-error")
    return(x)
  y
  
}

as.list.tsys <- function(x, ...){
  class(x) <- NULL
  x
}

##
## New Generic
##

as.tradesys <- function(x, ...){
  UseMethod("as.tradesys", x)
}

as.tradesys.default <- function(x, ...){
  if(is.list(x))
    return(do.call("tradesys", x))
  else
    stop("the default method coerces only lists.")
}
