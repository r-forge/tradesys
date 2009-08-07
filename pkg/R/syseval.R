syseval <- function(x, data, order.by=index(data)){
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
  St <- do.call("signalmap", c(Signals, x["entrycond"], x["entrywins"]))
  St <- cbind(St, data)[, 1]
  ## Evaluate delta, roll.at, size.at
  Frame <- as.list(as.data.frame(data))
  Frame$St <- St
  Frame$index <- order.by
  delta <- cbind(eval(x$delta, Frame), data)[, 1]
  size.at <- cbind(eval(x$size.at, Frame), data)[, 1]
  roll.at <- cbind(eval(x$roll.at, Frame), data)[, 1]
  ## Calculate equity
  Equity <- equity(prices(cbind(data, St), x$pricecols), St, delta, size.at, roll.at, x$percent)[, "Equity"]
  list(St=St, Equity=Equity, el=Signals$el, es=Signals$es, xl=Signals$xl, xs=Signals$xs,
       delta=delta, size.at=size.at, roll.at=roll.at, formulae=dataf)
}
