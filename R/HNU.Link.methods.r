 #' @title Extract Method
#' @name $
#' @aliases $,Link-method 
#' @rdname extract-methods 
setMethod("$","Link",function(x,name) {return(slot(x,name))})
 #' @title Set Method 
#' @name $<-
#' @aliases $<-,Link-method 
#' @rdname set-methods
setMethod("$<-","Link",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 
 