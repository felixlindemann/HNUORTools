
##############################################################
#' @name plotGeoSituation 
#' @docType methods
#' @export  
#' @rdname plotGeoSituation 
#'
#' @title Plot an Object of Type \code{\link{GeoSituation}}
#' @description This method should simplify the use of HNUORTools.
#'  
#' @param object an object of class \code{\link{GeoSituation}}
#' @param ... Additional argument list that might not ever be used.
#' @return The updated object of class \code{\link{GeoSituation}}.
#' @note 
#'      for citing use: Felix Lindemann (2014). HNUORTools: Operations Research Tools. R package version 1.1-0. \url{http://felixlindemann.github.io/HNUORTools/}.
#'      
#' @author Dipl. Kfm. Felix Lindemann \email{felix.lindemann@@hs-neu-ulm.de} 
#' 
#' Wissenschaftlicher Mitarbeiter
#' Kompetenzzentrum Logistik
#' Buro ZWEI, 17
#'
#' Hochschule fur angewandte Wissenschaften 
#' Fachhochschule Neu-Ulm | Neu-Ulm University 
#' Wileystr. 1 
#' 
#' D-89231 Neu-Ulm 
#' 
#' 
#' Phone   +49(0)731-9762-1437 
#' Web      \url{www.hs-neu-ulm.de/felix-lindemann/} 
#'      \url{http://felixlindemann.blogspot.de}
setGeneric("plotGeoSituation",            function(object,...)  standardGeneric("plotGeoSituation") )
setGeneric("plotGeoSituation.nodes",      function(object,...)  standardGeneric("plotGeoSituation.nodes") )
setGeneric("plotGeoSituation.links",      function(object,...)  standardGeneric("plotGeoSituation.links") )
setGeneric("plotGeoSituation.warehouses", function(object,...)  standardGeneric("plotGeoSituation.warehouses") )
setGeneric("plotGeoSituation.customers",  function(object,...)  standardGeneric("plotGeoSituation.customers") )
setGeneric("plotGeoSituation.warehouse",  function(object,...)  standardGeneric("plotGeoSituation.warehouse") )
setGeneric("plotGeoSituation.customer",   function(object,...)  standardGeneric("plotGeoSituation.customer") )
#' @aliases plotGeoSituation,GeoSituation-method
#' @rdname plotGeoSituation 
setMethod("plotGeoSituation",signature(object="GeoSituation"),
  function(object,...){ 
    
    li<-list(...)
    if(is.null(li$xlim)){#        
        li$xlim <- range(
            c(  sapply(object$nodes, function(o){o$x}),
                sapply(object$warehouses, function(o){o$x}),
                sapply(object$customers, function(o){o$x})
            ) 
        ) + c(-10,10)
    }#
    if(is.null(li$ylim)){#        
        li$ylim <- range(
            c(  sapply(object$nodes, function(o){o$y}),
                sapply(object$warehouses, function(o){o$y}),
                sapply(object$customers, function(o){o$y})
            ) 
        ) + c(-10,10)
    }#
    if(is.null(li$xlab)) li$xlab <- ""
    if(is.null(li$ylab)) li$ylab <- ""
    if(is.null(li$main)) li$main <- ""

    if(is.null(li$plotBorders)) li$plotBorders<- FALSE
    if(is.null(li$plotCities))  li$plotCities <- FALSE

    if(is.null(li$plotNodes)) li$plotNodes<- TRUE
    if(is.null(li$plotLinks)) li$plotLinks<- TRUE
    if(is.null(li$plotCustomers)) li$plotCustomers<- TRUE
    if(is.null(li$plotWarehouses)) li$plotWarehouses<- TRUE
    if(is.null(li$plotGrid)) li$plotGrid<- TRUE 

    if(is.null(li$grid.lty)) li$grid.lty<- c(1,2)
    if(is.null(li$grid.col)) li$grid.col<- c("grey","grey")

    if(is.null(li$grid.x)) li$grid.x<- c(50,10)
    if(is.null(li$grid.y)) li$grid.y<- c(50,10)
    if(is.null(li$grid.x.range)) li$grid.x.range<- c(0,180)
    if(is.null(li$grid.y.range)) li$grid.y.range<- c(0,180)

    if(length(li$plotGrid) ==1) li$plotGrid <- rep(li$plotGrid,2)
    if(length(li$grid.lty) ==1) li$grid.lty <- rep(li$grid.lty,2)
    if(length(li$grid.col) ==1) li$grid.col <- rep(li$grid.col,2)
    if(length(li$grid.x) ==1) li$grid.x <- rep(li$grid.x,2)
    if(length(li$grid.y) ==1) li$grid.y <- rep(li$grid.y,2)

    if(li$plotBorders){

      plotGeoSitatuon.bordersgermany( ... ) 
  
    }else{

      plot(NULL,NULL, #
          xlim = li$xlim, #
          ylim = li$ylim,#
          xlab=li$xlab,#
          ylab=li$ylab,#
          main=li$main,#
          sub=li$sub#
          #...
      )#
    }
    for(i in 2:1){
      if(li$plotGrid[i]){

        for(x in seq(li$grid.x.range[1],li$grid.x.range[2],by = li$grid.x[i])){
          abline(v=x, lty=li$grid.lty[i], col=li$grid.col[i])
        }
        for(y in seq(li$grid.y.range[1],li$grid.y.range[2],by = li$grid.y[i])){
          abline(h=y, lty=li$grid.lty[i], col=li$grid.col[i])
        }

      }
    }
    if(li$plotLinks) plotGeoSituation.links(object, ...)
    if(li$plotNodes) plotGeoSituation.nodes(object, ...)
    if(li$plotWarehouses) plotGeoSituation.warehouses(object, ...)
    if(li$plotCustomers) plotGeoSituation.customers(object, ...)
  }
)

setMethod("plotGeoSituation.nodes",signature(object="GeoSituation"),
  function(object,...){
    li<-list(...)
    n<-length(object$nodes)
    if(n>0){
      if(is.null(li$pch)) li$pch<- 21
      if(is.null(li$p.cex)) li$p.cex<- 3
      if(is.null(li$t.cex)) li$t.cex<- 0.75
      if(is.null(li$bg))  li$bg<- "white"
      if(is.null(li$col)) li$col<- 1
      if(is.null(li$withlabels)) li$withlabels<- TRUE
      x <- sapply(object$nodes, function(o){o$x})#
      y <- sapply(object$nodes, function(o){o$y})#
      points(x,y,pch = li$pch, bg = li$bg , cex=li$p.cex)#
      if(li$withlabels){
        text(x,y, sapply(object$nodes, function(o){o$id}), cex=li$t.cex,col=li$col)#
      }
    }
  }
)

setMethod("plotGeoSituation.links",signature(object="GeoSituation"),
  function(object,...){
    li<-list(...)
    n<-length(object$links)
    if(n>0){

      if(is.null(li$pch)) li$pch<- 21
      if(is.null(li$p.cex)) li$lines.p.cex<- 2
      if(is.null(li$t.cex)) li$lines.t.cex<- 0.5
      if(is.null(li$lwd)) li$lwd<- 1
      if(is.null(li$bg))  li$bg<- "white" 
      if(is.null(li$lines.markused)) li$lines.markused<- TRUE
      if(is.null(li$lines.plotlength)) li$lines.plotlength<- FALSE
      if(is.null(li$lwd.used)) li$lwd.used<- li$lwd*3
      if(is.null(li$lty)) li$lty<- 1 
      if(is.null(li$col)) li$col<- 1 
      if(is.null(li$col.used)) li$col.used<- "red"

      for(i in 1:n){
          link <- object$links[[i]]
          x<- c(link$origin$x, link$destination$x)
          y<- c(link$origin$y, link$destination$y)
          if(link$used & li$lines.markused){ 
            lines(x,y,lty=li$lty, lwd=li$lwd.used, col=li$col.used)#
          }
            lines(x,y,lty=li$lty, lwd=li$lwd, col=li$col)#
          if(li$lines.plotlength){
            x <- sum(x)/2
            y <- sum(y)/2
            points(x,y,pch = li$pch, bg = li$bg , cex=li$lines.p.cex)# 
            text(x,y, link$distance, cex=li$lines.t.cex,col=li$col)#
          }
      }
    }
  }
)
setMethod("plotGeoSituation.customers",signature(object="GeoSituation"),
  function(object,...){ 
    li<-list(...)
    n<-length(object$customers)
    if(n>0){
      if(is.null(li$zoom )) li$zoom <- .5
      if(is.null(li$bg.col )) li$bg.col <- "gray"
      if(is.null(li$border.col )) li$border.col <- "black"
      if(is.null(li$font.cex )) li$font.cex <- 1
      if(is.null(li$font.col )) li$font.col <- "black"
      if(is.null(li$point.pch )) li$point.pch <- 20
      if(is.null(li$point.cex )) li$point.cex <- 1#
      if(is.null(li$withlabels)) li$withlabels<- TRUE

      if(length(li$bg.col) != n) li$bg.col <- rep(li$bg.col, n)
      if(length(li$border.col) != n) li$border.col <- rep(li$border.col, n)
      if(length(li$font.col) != n) li$font.col <- rep(li$font.col, n)
      if(length(li$point.pch) != n) li$point.pch <- rep(li$point.pch, n)

      for(i in 1:n){
          customer <- object$customers[[i]]
          plotGeoSituation.customer(customer,
                zoom=li$zoom,
                bg.col=li$bg.col[i],
                border.col=li$border.col[i],
                font.cex=li$font.cex,
                font.col=li$font.col[i],
                point.pch=li$point.pch[i],
                point.cex=li$point.cex,
                withlabels = li$withlabels
            )
      }
    }
  }
)
setMethod("plotGeoSituation.customer",signature(object="Customer"),
  function(object,...){ 
    li<-list(...)
    if(is.null(li$zoom ))       li$zoom <- .5
    if(is.null(li$bg.col ))     li$bg.col <- "gray"
    if(is.null(li$border.col )) li$border.col <- "black"
    if(is.null(li$font.cex ))   li$font.cex <- 1
    if(is.null(li$font.col ))   li$font.col <- "black"
    if(is.null(li$point.pch ))  li$point.pch <- 20
    if(is.null(li$point.cex ))  li$point.cex <- 1#
    if(is.null(li$withlabels))  li$withlabels<- TRUE

    dx<-(c(0,0,-0.5,1,2.5,2,2,0)-1)*li$zoom + object$x#
    dy<-(c(0,2,2,3.5,2,2,0,0)-1.5) *li$zoom + object$y#

    polygon(dx, dy, col=li$bg.col, border = li$border.col)#
    points(object$x,object$y,pch=li$point.pch,cex=li$point.cex )#
    if(li$withlabels){
      text(max(dx),min(dy)-1.5*li$zoom, object$id, cex=li$font.cex,col=li$font.col)#
    }
  }
)
setMethod("plotGeoSituation.warehouses",signature(object="GeoSituation"),
  function(object,...){ 
    li<-list(...)
    n<-length(object$warehouses)
    if(n>0) { 
      if(is.null(li$zoom )) li$zoom <- .5
      if(is.null(li$warehouses.bg.col )) li$warehouses.bg.col <- "gray"
      if(is.null(li$border.col )) li$border.col <- "black"
      if(is.null(li$font.cex )) li$font.cex <- 1
      if(is.null(li$font.col )) li$font.col <- "black"
      if(is.null(li$point.pch )) li$point.pch <- 20
      if(is.null(li$point.cex )) li$point.cex <- 1#
      if(is.null(li$withlabels)) li$withlabels<- TRUE

      if(length(li$warehouses.bg.col) != n) li$warehouses.bg.col <- rep(li$warehouses.bg.col, n)
      if(length(li$border.col) != n) li$border.col <- rep(li$border.col, n)
      if(length(li$font.col) != n) li$font.col <- rep(li$font.col, n)
      if(length(li$point.pch) != n) li$point.pch <- rep(li$point.pch, n)
      if(is.null(li$isWLP)) li$isWLP <- FALSE
      for(i in 1:n){
          warehouse <- object$warehouses[[i]]
          plotGeoSituation.warehouse(warehouse,#
                zoom=li$zoom,
                warehouses.bg.col=li$warehouses.bg.col[i],
                border.col=li$border.col[i],
                font.cex=li$font.cex,
                font.col=li$font.col[i],
                point.pch=li$point.pch[i],
                point.cex=li$point.cex,
                withlabels =li$withlabels,
                isWLP=  li$isWLP
            )
      }
    }
  }
)
setMethod("plotGeoSituation.warehouse",signature(object="Warehouse"),
  function(object,...){ 
    li<-list(...)
    if(is.null(li$zoom )) li$zoom <- .5
    if(is.null(li$warehouses.bg.col )) li$warehouses.bg.col <- "grey"
    if(is.null(li$border.col )) li$border.col <- "black"
    if(is.null(li$font.cex )) li$font.cex <- 1
    if(is.null(li$font.col )) li$font.col <- "black"
    if(is.null(li$point.pch )) li$point.pch <- 20
    if(is.null(li$point.cex )) li$point.cex <- 1#
    if(is.null(li$withlabels)) li$withlabels<- TRUE
    if(is.null(li$isWLP)) li$isWLP <- FALSE

    if(li$isWLP & object$open ==0) li$warehouses.bg.col <- "white"

    dx<-(c(0,0,1,1,2,2,3,3,4,4,5,5,0)-2.5)*li$zoom + object$x#
    dy<-(c(0,3.5,2,3.5,2,3.5,2,3.5,2,5,5,0,0)-1.5) *li$zoom + object$y#
    polygon(dx, dy, col=li$warehouses.bg.col, border =li$border.col)#
    points(object$x,object$y,pch=li$point.pch,cex=li$point.cex )#
    if(li$withlabels){
      text(max(dx),min(dy)-1.5*li$zoom, object$id, cex=li$font.cex,col=li$font.col)#
    }
  }
)
 