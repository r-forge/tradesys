test.inside <- function(){

  ## Error: "names(exprlist) must be non-null and unique."  
  checkException(inside(spx, list(quote(Close * 2), quote(Open * 3))), silent=TRUE) # names(exprlist) is NULL
  checkException(inside(spx, list(quote(Close * 2), A=quote(Open * 3))), silent=TRUE) # names(exprlist) contains ""
  checkException(inside(spx, list(A=quote(Close * 2), A=quote(Open * 3))), silent=TRUE) # names(exprlist) contains dupes
  checkException(inside(spx, list(A=quote(Close * 2), Close=quote(Open * 3))), silent=TRUE) # a name matches names(data)

  ## Error: "data must be a list or a data.frame."
  checkException(inside(c(Keats=1:10, Shelly=11:20), list(Byron=quote(Keats / 2))), silent=TRUE)

  ## Return is a list or a dataframe, depending upon what is passed to data.
  checkEquals(class(inside(list(Keats=1:10, Shelly=11:20), list(Byron=quote(Keats / 2)))), "list") 
  checkEquals(class(inside(data.frame(Keats=1:10, Shelly=11:20), list(Byron=quote(Keats / 2)))), "data.frame")

  ## Expressions may evaluate to different lengths when data is a list.
  checkEquals(inside(list(Keats=1:10, Shelly=11:20), list(Byron=quote(Shelly[1:3])))$Byron, 11:13)

  ## ..But must be a multiple of nrow(data) when data is a data.frame.
  checkEquals(inside(data.frame(Keats=1:10, Shelly=11:20), list(Byron=quote(1:5)))$Byron, c(1:5,1:5))
  checkException(inside(data.frame(Keats=1:10, Shelly=11:20), list(Byron=quote(1:3)))$Byron, silent=TRUE)

  ## exprlist evaluations are appended to the right of data and data is left unchanged.
  checkEquals(inside(spx, list(A=quote(Close * .99), B=quote(Close * 1.01)))[, 1:5], spx)
  
}


