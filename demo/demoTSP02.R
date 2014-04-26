geo<-new("GeoSituation")    
data(bordersgermany)
#chemnitz, cottbus, dresden, erfurt, gera, halle leipzig
A<- c(10, 11, 14, 17, 24, 22, 35)
for(a in A){
  df <- cities[a, ]
  geo<-add(geo,new("Node", id=df$id, label = df$label, x=df$x,   y=df$y))
}
t.costs <- 1
s.node <- 3 # index of dresden in geo$nodes

geo$tsp<- TSP.NearestNeighbor(geo, nodes=geo$nodes, StartNode = s.node)


plotGeoSituation(
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
text(cities[A,"x"],cities[A,"y"]+5, cities[A,"id"],col=4)

#2opt
geo$tsp<-TSP.3OPT(geo)

plotGeoSituation(
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
text(cities[A,"x"],cities[A,"y"]+5, cities[A,"id"],col=4)