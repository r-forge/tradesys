tsys.frame <- function(x, data, order.by=index(data)){
  ## validate data and order.by
  if(any(duplicated(order.by)))
    stop("all order.by must be unique.")
  if(is.matrix(data))
    data <- as.list(as.data.frame(data))
  else
    data <- as.list(data)
  if(any(!x$datavars %in% names(data)))
    stop("data must contain all variables named in x$datavars")
  data <- data[x$datavars]
  if(length(unique(lapply(data, length))) > 1)
    stop("all variables in data must be the same length")
  if(any(!unlist(lapply(data, is.numeric))))
    stop("all data variables must be numeric")
  if(length(order.by) != length(data[[1]]))
    stop("order.by must have the same length as the variables in data")
  ## Create the evaluation environment and assign data and index to it
  env <- new.env(parent=.GlobalEnv)
  for(i in seq(1, length(data)))
    assign(names(data)[i], data[[i]], envir=env)
  assign("index", order.by, envir=env)
  ## Evaluate and assign signals in env
  Signals <- lapply(c(x["el"], x["es"], x["xl"], x["xs"]), eval, env)
  Signals <- lapply(Signals, function(x, y) as.logical(cbind(x, y)[, 1]), data[[1]])
  states <- do.call("signalmap", c(Signals, x["entrycond"], x["entrywins"]))
  states <- cbind(states, data[[1]])[, 1]
  for(i in seq(1, length(Signals)))
    assign(names(Signals)[i], Signals[[i]], envir=env)
  assign("states", states, envir=env)
  ## Evaluate exprvars
  if(!is.null(x$exprvars)){
    expv <- lapply(x$exprvars, eval, envir=env)
    for(i in seq(1, length(expv)))
      assign(names(expv)[i], expv[[i]], envir=env)
  }
  ## Evaluate and assign roll.at in env
  roll.at <- eval(x$roll.at, env)
  assign("roll.at", roll.at, envir=env)
  ## Call prices and assign its variables in env
  prices <- prices(do.call("cbind", data), states, x$pricemap)
  Prices <- as.list(as.data.frame(prices))
  for(i in seq(1, length(Prices)))
    assign(names(Prices)[i], Prices[[i]], envir=env)
  ## Evaluate and assign size.at in env
  size.at <- eval(x$size.at, env)
  assign("size.at", size.at, envir=env)
  ## Evaluate and assign delta in env
  delta <- eval(x$delta, env)
  assign("delta", delta, envir=env)
  ## Call equity and assign equity in env
  equity <- equity(prices[, 1:2], states, delta, size.at, roll.at, x$percent)[, "Equity"]
  d <- data.frame(states, equity, el=Signals$el, es=Signals$es, xl=Signals$xl, xs=Signals$xs,
                  delta=delta, size.at=size.at, roll.at=roll.at)
  if(!is.null(x$exprvars))
    d <- cbind(d, do.call("cbind", expv))
  d
}
