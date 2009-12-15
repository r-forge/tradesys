choose.prices <- function(states, uc, chg=uc, ent=chg, xit=chg, lng=chg, sht=chg,
                          el=if(identical(ent, chg)) lng else ent,
                          es=if(identical(ent, chg)) sht else ent,
                          xl=if(identical(xit, chg)) sht else xit,
                          xs=if(identical(xit, chg)) lng else xit){
  if(!all(states %in% c(1,0,-1)))
    stop("states must be a vector consisting of 1, 0, and -1 values.")
  m <- cbind(ph=phasemap(states), uc, xs, xl, es, el)
  x <- m[, "uc"]
  x[which(m[, "ph"] == 1)] <- m[which(m[, "ph"] == 1), "xs"]
  x[which(m[, "ph"] == 2)] <- m[which(m[, "ph"] == 2), "xl"]
  x[which(m[, "ph"] == 4)] <- m[which(m[, "ph"] == 4), "es"]
  x[which(m[, "ph"] == 8)] <- m[which(m[, "ph"] == 8), "el"]
  x
}

