prices <- function(x){
  if(!is.tsts(x))
    stop("x must be class 'tsts'.")
  y <- as.matrix(x)[, unlist(lapply(attr(x, "tsts")$pricecols, function(x, y) if(is.numeric(x)) colnames(y)[x] else x, x))]
  colnames(y) <- names(attr(x, "tsts")$pricecols)
  y <- cbind(using=y[, 1], y)
  ## NOTE: 
  ## The entershort price column will be used for XL phases where s(t)=-1 and s(t-1)=1.
  ## The enterlong price column will be used for XS phases where s(t)=1 and s(t-1)=-1.
  h <- phases(x)
  y[which(h == "EL"), "using"] <- y[which(h == "EL"), "enterlong"]
  y[which(h == "ES"), "using"] <- y[which(h == "ES"), "entershort"]
  y[which(h == "XL"), "using"] <- y[which(h == "XL"), "exitlong"]
  y[which(h == "XS"), "using"] <- y[which(h == "XS"), "exitshort"]
  y
}
