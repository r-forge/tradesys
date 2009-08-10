tsys.frame <- function(x, data, order.by=index(data)){
  if(any(duplicated(order.by)))
    stop("all order.by must be unique.")
  data <- as.matrix(data)
  rownames(data) <- NULL
  ## Evaluate formulae
  dataf <- do.call("cbind", lapply(x$formulae, eval, as.list(as.data.frame(data))))
  data <- cbind(data, dataf)
  ## Evaluate signals
  Frame <- as.list(as.data.frame(data))
  Frame$index <- order.by
  Signals <- lapply(c(x["el"], x["es"], x["xl"], x["xs"]), eval, Frame)
  Signals <- lapply(Signals, function(x, y) as.logical(cbind(x, y)[, 1]), data)
  states <- do.call("signalmap", c(Signals, x["entrycond"], x["entrywins"]))
  states <- cbind(states, data)[, 1]
  ## Evaluate delta, roll.at, size.at
  Frame <- as.list(as.data.frame(data))
  Frame$states <- states
  Frame$index <- order.by
  delta <- cbind(eval(x$delta, Frame), data)[, 1]
  size.at <- as.logical(cbind(eval(x$size.at, Frame), data)[, 1])
  roll.at <- as.logical(cbind(eval(x$roll.at, Frame), data)[, 1])
  ## Calculate equity
  equity <- equity(prices(cbind(data, states), x$pricemap), states, delta, size.at, roll.at, x$percent)[, "Equity"]
  d <- data.frame(states, equity, el=Signals$el, es=Signals$es, xl=Signals$xl, xs=Signals$xs,
                  delta=delta, size.at=size.at, roll.at=roll.at)
  if(!is.null(dataf))
    d <- cbind(d, dataf)
  d
}
