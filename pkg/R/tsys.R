tradesys <- function(datavars, pricemap=c(Mark=datavars[1]), el=FALSE, es=FALSE, 
                     xl=FALSE, xs=FALSE, delta=1, size.at=as.logical(c(states[1], diff(states))),
                     roll.at=FALSE, formulae=NULL, percent=TRUE, entrywins=FALSE,
                     entrycond=FALSE){
  l <- list()
  ## Process datavars
  if(any(duplicated(datavars)))
    stop("datavars be unique.")
  l$datavars <- datavars
  ## Process pricemap arg
  if(!is.vector(pricemap) | !is.character(pricemap))
    stop("pricemap must be a character vector of column names.")
  if(length(pricemap) > 5)
    stop("length(pricemap) must be <= 5")
  if(is.null(names(pricemap))) ## .. then order determines mapping when no names
    names(pricemap) <- c("Mark","Long","Short","RollLong","RollShort")[1:length(pricemap)]
  if(any(duplicated(names(pricemap))))
    stop("names(pricemap) must be unique")
  if(any(!names(pricemap) %in% c("Mark","Long","Short","RollLong","RollShort")))
    stop("All names(pricemap) must be among 'Mark','Long','Short','RollLong','RollShort'")
  if(any(!pricemap %in% datavars))
    stop("all pricemap must be in datavars or names(formulae)")
  l$pricemap <- c(pricemap["Mark"], pricemap["Long"], pricemap["Short"], pricemap["RollLong"], pricemap["RollShort"])
  names(l$pricemap) <- c("Mark","Long","Short","RollLong","RollShort")
  if(is.na(l$pricemap["Mark"]))
    stop("pricemap must be passed a value for 'Mark'")
  if(is.na(l$pricemap["Long"]))
    l$pricemap["Long"] <- l$pricemap["Mark"]
  if(is.na(l$pricemap["Short"]))
    l$pricemap["Short"] <- l$pricemap["Mark"]
  if(is.na(l$pricemap["RollShort"]))
    l$pricemap["RollShort"] <- l$pricemap["Short"]
  if(is.na(l$pricemap["RollLong"]))
    l$pricemap["RollLong"] <- l$pricemap["Long"]
  ## Process formulae arg (we expect a named list of calls)
  if(!is.null(formulae)){
    if(!is.list(formulae) | is.null(names(formulae)) | any(duplicated(names(formulae))) | any(names(formulae) %in% datavars))
      stop("formulae must be a list with names that are unique and not in datavars")
    if(any(unlist(lapply(formulae, class)) != "call"))
      stop("formulae must be a list of call objects")
    l$formulae <- formulae
  }else{
    l["formulae"] <- list(NULL)
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
  if(!is.null(x$formulae)){
    cat("** formulae **\n")
    for(i in 1:length(x$formulae))
      cat(names(x$formulae)[i],":", format(x$formulae[[i]]), "\n")
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
    stop("the default method currently coerces only lists.")
}
