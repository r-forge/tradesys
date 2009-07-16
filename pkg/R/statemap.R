statemap <- function(phases){
  if(any(!phases %in% c("EL","ES","XA","XL","XS","UC")))
    stop("phases must be a vector of 'EL','ES','XA','XL','XS' and 'UC'")
  phases <- toupper(phases)
  x <- rep(NA, length(phases))
  x[which(phases == "EL")] <- 1
  x[which(phases == "ES")] <- -1
  x[which(phases == "XA")] <- 0
  if(is.na(x[1]))
    x[1] <- 0
  for(i in which(is.na(x))){
    x[i] <- x[i-1]
    if(phases[i] == "XL" & x[i-1] == 1)
      x[i] <- 0
    if(phases[i] == "XS" & x[i-1] == -1)
      x[i] <- 0
  }
  x
}
