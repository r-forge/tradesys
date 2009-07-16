signals <- function(el=FALSE, es=FALSE, xl=FALSE, xs=FALSE, entrycond=FALSE, entrywins=FALSE){
  el[which(is.na(el))] <- FALSE
  es[which(is.na(es))] <- FALSE
  xl[which(is.na(xl))] <- FALSE
  xs[which(is.na(xs))] <- FALSE
  if(entrycond){
    if(any(xl)){
      for(i in which(xl))
        el[i:min(which(j <- which(es) > i)[j])] <- FALSE
    }
    if(any(xs)){
      for(i in which(xs))
        es[i:min(which(j <- which(el) > i)[j])] <- FALSE
    }
  }
  l <-lapply(list(el, es, xl, xs), length)
  if(any(!unlist(lapply(l, function(x, m){if(x > 1 & x != m) FALSE else TRUE}, max(unlist(l))))))
    stop("el, es, xl, and xs must all be the same length.")
  x <- cbind(el, es, xl, xs)
  x <- apply(matrix(x, ncol=4), 2, function(x){x[which(is.na(x))] <- 0; x})
  PHASES <- c("UC","XS","XL","XA","ES","XS","ES","XA","EL","EL","XL","XA")
  if(entrywins){
    PHASES[c(6,8)] <- "ES"
    PHASES[c(11,12)] <- "EL"
  }
  y <- apply(matrix(x, ncol=4), 1, function(x){sum(c(8,4,2,1) * x)})
  if(any(y > 11))
    stop("el and es cannot both be TRUE.")
  statemap(PHASES[y+1])
}

## el/es/xl/xs
## ----------------
## 0000 = 0  --> UC 
## 0001 = 1  --> XS 
## 0010 = 2  --> XL
## 0011 = 3  --> XA
## 0100 = 4  --> ES
## 0101 = 5  --> XS*
## 0110 = 6  --> ES
## 0111 = 7  --> XA*
## 1000 = 8  --> EL
## 1001 = 9  --> EL
## 1010 = 10 --> XL*
## 1011 = 11 --> XA*
## 1100 = 12 --> --
## 1101 = 13 --> --
## 1110 = 14 --> --
## 1111 = 15 --> --
## ----------------
## * EL or ES if !entrywins 

