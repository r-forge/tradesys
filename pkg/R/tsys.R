tradesys <- function(datacols, pricecols=datacols[1], el=FALSE, es=FALSE, 
                     xl=FALSE, xs=FALSE, delta=1, size.at=as.logical(c(St[1], diff(St))),
                     roll.at=FALSE, formulae=NULL, percent=TRUE, entrywins=FALSE,
                     entrycond=FALSE){
  l <- list()
  ## Process datacols
  if(any(duplicated(datacols)))
    stop("datacols be unique.")
  l$datacols <- datacols
  ## Process pricecols arg
  if(!is.vector(pricecols) | !is.character(pricecols))
    stop("pricecols must be a character vector of column names.")
  pricecols <- pricecols[1:min(length(pricecols), 5)]
  if(length(pricecols) == 1) ## everyone gets what's passed
    names(pricecols) <- NULL
  if(is.null(names(pricecols))) ## order determines mapping
    names(pricecols) <- c("Mark","Long","Short","RollLong","RollShort")[1:length(pricecols)]
  if(any(!names(pricecols) %in% c("Mark","Long","Short","RollLong","RollShort")))
    stop("All names(pricecols) must be among 'Mark','Long','Short','RollLong','RollShort'")
  if(any(!pricecols %in% datacols))
    stop("all pricecols must be in datacols or names(formulae)")
  l$pricecols <- c(pricecols["Mark"], pricecols["Long"], pricecols["Short"], pricecols["RollLong"], pricecols["RollShort"])
  names(l$pricecols) <- c("Mark","Long","Short","RollLong","RollShort")
  if(is.na(l$pricecols["Mark"]))
    stop("pricecols must be passed a value for 'Mark'")
  if(is.na(l$pricecols["Long"]))
    l$pricecols["Long"] <- l$pricecols["Mark"]
  if(is.na(l$pricecols["Short"]))
    l$pricecols["Short"] <- l$pricecols["Mark"]
  if(is.na(l$pricecols["RollShort"]))
    l$pricecols["RollShort"] <- l$pricecols["Short"]
  if(is.na(l$pricecols["RollLong"]))
    l$pricecols["RollLong"] <- l$pricecols["Long"]
  ## Process formulae arg (we expect a named list of calls)
  if(!is.null(formulae)){
    if(!is.list(formulae) | is.null(names(formulae)) | any(duplicated(names(formulae))) | any(names(formulae) %in% datacols))
      stop("formulae must be a list with names that are unique and not in datacols")
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
  cat("datacols: ", x$datacols, "\n")
  cat("pricecols:", x$pricecols, "\n")
  cat("el:", format(x$el), "\n")
  cat("es:", format(x$es), "\n")
  cat("xl:", format(x$xl), "\n")
  cat("xs:", format(x$xs), "\n")
  if(!is.null(x$formulae)){
    cat("** formulae **\n")
    for(i in 1:length(x$formulae))
      cat(names(x$formulae)[i],":", format(x$formulae[[i]]), "\n")
  }
  cat("delta:  ", format(x$delta), "\n")
  cat("size.at:", format(x$size.at), "\n")
  cat("roll.at:", format(x$roll.at), "\n")
  cat("percent:  ", format(x$percent), "\n")
  cat("entrywins:", format(x$entrywins), "\n")
  cat("entrycond:", format(x$entrycond), "\n")
}

"$<-.tsys" <- function(x, name=NULL, value){
  y <- x
  if(is.null(name))
    y <- value
  else
    y[[name]] <- value
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
