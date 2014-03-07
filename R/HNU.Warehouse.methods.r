#' @title Extract Method
#' @name $
#' @aliases $,Warehouse-method 
#' @rdname extract-methods 
setMethod("$","Warehouse",function(x,name) {return(slot(x,name))})
  #' @title Set Method 
#' @name $<-
#' @aliases $<-,Warehouse-method 
#' @rdname set-methods
setMethod("$<-","Warehouse",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 