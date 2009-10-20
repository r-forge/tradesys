tradesys.frame <- function(x, data, order.by=index(data)){
  Index <- order.by
  data <- as.list(as.data.frame(data))
  f <- function(){
    l <- list()
    for(i in names(formals())){
      l[[i]] <- eval(as.name(i))
    }
    l
  }
  formals(f) <- c(as.list(x), data)
  cbind(Index, as.data.frame(f()))
}
