HNU.GeografischeSituation <- setRefClass("HNU.GeografischeSituation",
  fields = list( 
    Knoten   = "list",
    Kanten   = "list",
    Kunden   = "list",
    Lager    = "list"
  ),
  methods = list(
    isValid = function(){
      return(TRUE)
    },
    initRandomKnoten = function(n, seed = 1){
      set.seed(seed)
      .self$Knoten <- list()
      for(i in 1:n){
        k <- HNU.Knoten$new(
          id = paste("k",i,sep=""),
          x = runif(1,0,100),
          y = runif(1,0,100),
          label = paste("Knoten",i)
        )
        .self$addKnoten(k)
      }
    },
    addKnoten = function(knoten){
      if(missing(knoten)){
        simpleError(paste("Fehler. Das Argument 'knoten' fehlt. Die Operation ist daher nicht möglich."))
      }
       
      .self$Knoten[[length(.self$Knoten)+1]] <- knoten 
    },
    addKante = function(i,j,Kostensatz=1,oneway=FALSE){
      if(missing(i)){
        simpleError(paste("Fehler. Das Argument 'from' fehlt. Die Operation ist daher nicht möglich."))
      }
      if(missing(j)){
        simpleError(paste("Fehler. Das Argument 'to' fehlt. Die Operation ist daher nicht möglich."))
      }
      
      l1 <- HNU.Kante$new(
        from = .self$Knoten[[i]],
        to   = .self$Knoten[[j]],
        oneWay = oneway
      )
      l1$setDistance(Kostensatz)
      .self$Kanten[[length(.self$Kanten)+1]] <- l1  
    },
    addKunde = function(kunde){
      if(missing(kunde)){
        simpleError(paste("Fehler. Das Argument 'kunde' fehlt. Die Operation ist daher nicht möglich."))
      } 
      .self$Kunden[[length(.self$Kunden)+1]] <- kunde 
    },
    addLager = function(lager){
      if(missing(lager)){
        simpleError(paste("Fehler. Das Argument 'lager' fehlt. Die Operation ist daher nicht möglich."))
      }
      
      .self$Lager[[length(.self$Lager)+1]] <- lager 
    }, 
    plotGeoSituation = function(xlim,ylim,xlab="",ylab="",main=""){
      x <- sapply(.self$Knoten, function(o){o$x})
      y <- sapply(.self$Knoten, function(o){o$y})
      if(missing(xlim )){
        xlim <- range(x)
      }
      if(missing(ylim )){
        ylim <- range(y)
      }
      plot(x,y,pch=20, 
          xlim = xlim, 
          ylim = ylim,
          xlab=xlab,
          ylab=ylab,
          main=main
      )


      #Draw links
      for(i in 1:length(Kanten)){

        l <- Kanten[[i]]

        a <- c(l$from$x, l$to$x)
        b <- c(l$from$y, l$to$y)
        lines(a,b,lty=1)
      }

      points(x,y,pch = 21,bg = "white" , cex=3)

      text(x,y, sapply(.self$Knoten, function(o){o$id}), cex=.6,col=1)

    },
    drawCustomer = function(customer,
      zoom=.5,
      bg.col="gray",
      border.col="black",
      font.cex=1,
      font.col="black",
      point.pch=20,
      point.cex=1)
    {
      dx<-(c(0,0,-0.5,1,2.5,2,2,0)-1)*zoom + customer$x
      dy<-(c(0,2,2,3.5,2,2,0,0)-1.5) *zoom + customer$y

      polygon(dx, dy, col=bg.col, border = border.col)
      points(customer$x,customer$y,pch=point.pch,cex=point.cex )
      text(max(dx),min(dy)-1*zoom, customer$label, cex=font.cex,col=font.col)
    },
    drawWarehouse = function(warehouse,
      zoom=.5,
      bg.col="white",
      border.col="black",
      font.cex=1,
      font.col="black",
      point.pch=20,
      point.cex=1)
    {
      dx<-(c(0,0,1,1,2,2,3,3,4,4,5,5,0)-2.5)*zoom+warehouse$x
      dy<-(c(0,3.5,2,3.5,2,3.5,2,3.5,2,5,5,0,0)-1.5)*zoom+warehouse$y

      polygon(dx, dy, col=bg.col, border = border.col)
      points(warehouse$x,warehouse$y,pch=point.pch ,cex=point.cex)
     
      text(max(dx),min(dy)-1*zoom, warehouse$label, cex=font.cex,col=font.col)
    }
  )
)

x<-HNU.GeografischeSituation$new()

x$initRandomKnoten(5)

x$addKante(1,2)
x$addKante(1,3)
x$addKante(2,3)
x$addKante(2,4)
x$addKante(3,4)
x$addKante(4,5)

x$plotGeoSituation()