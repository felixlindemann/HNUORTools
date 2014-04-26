set.seed(10) # 2,3,5 work as well
geo<-new("GeoSituation")
N<- 7
for(i in 1:N){
  geo<-add(geo,new("Node", id=paste("N",i,sep=""))) 
} 


for( i in 1:(N-1)){
  
  for( j in sample(1:N, sample(1:3,1), replace=TRUE )){
    # warnings can be ignored - as this is a sample SPP 
    geo<-add(geo,new("Link",geo$nodes[i],geo$nodes[j], digits =0))
    
  } 
}

startindex <- 6 #sample(1:N, 1)

geo <- SPP.Dijkstra(geo,start=startindex, log=FALSE)
plotGeoSituation(
  geo,
  main="random shortest path problem", 
  sub = paste("iteration:", geo$spp$iter),
  drawLinks = TRUE,
  drawNodes = TRUE,
  lines.plotlength = TRUE
)

#mark StartNode
points(geo$nodes[startindex]$x,geo$nodes[startindex]$y,pch = 21, bg = 2 , cex=4)

#redraw nodes
drawNodes(geo)
 