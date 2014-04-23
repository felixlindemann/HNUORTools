geo<-new("GeoSituation")    
data(bordersgermany) 
set.seed(1)
N<-25
A<- sample(1:57,N) 
startindex<- sample(1:N,1)
for(a in A){
  df <- cities[a, ]
  geo<-add(geo,new("Node", id=df$id, label = df$label, x=df$x,   y=df$y))
} 

geo$tsp<- TSP.NearestNeighbor(geo, nodes=geo$nodes, StartNode = startindex, digits=0)


plot(
  geo, 
  plotBorders=TRUE, 
  pch=20, 
  point.cex=1,
  xlim=range(cities[A, "x"]), 
  ylim=range(cities[A, "y"])+c(0,10),
  withlabels = FALSE,
  drawTSP=TRUE 
)
title(main="TSP: Nearest-Neighbor", sub=paste("F = ca.",round(geo$tsp$F), "km"))
# text(cities[A,"x"],cities[A,"y"]+5, cities[A,"id"],col=4)

F<- geo$tsp$F

while(TRUE){
  
  #2opt
  geo$tsp<-TSP.3OPT(geo, tsp = geo$tsp, 
                    maxiter =1 # deactivate if animation is not wanted
                    )
  
  plot(
    geo, 
    plotBorders=TRUE, 
    pch=20, 
    point.cex=1,
    xlim=range(cities[A, "x"]), 
    ylim=range(cities[A, "y"])+c(0,10),
    withlabels = FALSE,
    drawTSP=TRUE  
  )
  title(main="TSP: Nearest-Neighbor + 3Opt", sub=paste("F = ca.",round(geo$tsp$F), "km"))
  # text(cities[A,"x"],cities[A,"y"]+5, cities[A,"id"],col=4)
  if(  F > geo$tsp$F){ 
    F<- geo$tsp$F
  }else{
    break
  }

}