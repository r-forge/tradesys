choose.prices <- function(states, uc, chg=uc, ent=chg, xit=chg, lng=chg, sht=chg,
                          el=if(identical(ent, chg)) lng else ent,
                          es=if(identical(ent, chg)) sht else ent,
                          xl=if(identical(xit, chg)) sht else xit,
                          xs=if(identical(xit, chg)) lng else xit){
  if(!all(states %in% c(1,0,-1)))
    stop("states must be a vector consisting of 1, 0, and -1 values.")
  ## All vectors must be a multiple of length(states)
  uc <- cbind(uc, states)[, 1]
  el <- cbind(el, states)[, 1]
  es <- cbind(es, states)[, 1]
  xl <- cbind(xl, states)[, 1]
  xs <- cbind(xs, states)[, 1]
  m <- cbind(phasemap(states) + 1, uc, xs, xl, es, el)
  apply(m, 1, function(x){switch(x[1], x[2], x[3], x[4],, x[5], , , , x[6])})
}

