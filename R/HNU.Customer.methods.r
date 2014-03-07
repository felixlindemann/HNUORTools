 #' @title Extract Method
#' @name $
#' @aliases $,Customer-method 
#' @rdname extract-methods 
setMethod("$","Customer",function(x,name) {return(slot(x,name))})
  #' @title Set Method 
#' @name $<-
#' @aliases $<-,Customer-method 
#' @rdname set-methods
setMethod("$<-","Customer",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 