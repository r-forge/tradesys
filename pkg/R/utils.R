pricemapper <- function(pricemap){
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
  x <- c(pricemap["Mark"], pricemap["Long"], pricemap["Short"], pricemap["RollLong"], pricemap["RollShort"])
  names(x) <- c("Mark","Long","Short","RollLong","RollShort")
  if(is.na(x["Mark"]))
    stop("pricemap must be passed a value for 'Mark'")
  if(is.na(x["Long"]))
    x["Long"] <- x["Mark"]
  if(is.na(x["Short"]))
    x["Short"] <- x["Mark"]
  if(is.na(x["RollShort"]))
    x["RollShort"] <- x["Short"]
  if(is.na(x["RollLong"]))
    x["RollLong"] <- x["Long"]
  x
}
