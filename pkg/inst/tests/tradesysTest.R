test.tradesys <- function(){
  x <- tradesys()
  x$foo <- quote(sin(prices))

  checkEquals(class(x), "tradesys")

  ## 'tradesys' class is stripped if any formals are removed from the subsetted object.
  checkEquals(class(x[-which(names(x) == "prices")]), "list")
  checkEquals(class(x[-which(names(x) == "rollout")]), "list")
  checkEquals(class(x[-which(names(x) == "rollin")]), "list")
  checkEquals(class(x[-which(names(x) == "states")]), "list")
  checkEquals(class(x[-which(names(x) == "roll.at")]), "list")
  checkEquals(class(x[-which(names(x) == "size.at")]), "list")
  checkEquals(class(x[-which(names(x) == "equity")]), "list")
  checkEquals(class(x[-which(names(x) %in% c("foo","prices"))]), "list")

  ## ... otherwise, it is not.
  checkEquals(class(x[-which(names(x) == "foo")]), "tradesys")
}

