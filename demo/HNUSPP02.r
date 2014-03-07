  
set.seed(51) # 24,29,33
geo<-new("GeoSituation")
N<- 10
for(i in 1:N){
	geo<-add(geo,new("Node", id=paste("N",i,sep=""))) 
} 


for( i in 1:(N-1)){

	for( j in sample(1:N, sample(1:3,1), replace=TRUE )){


		geo<-add(geo,new("Link",geo$nodes[[i]],geo$nodes[[j]]))

	}

}
 geo <- SPP.Dijkstra(geo,start=1, log=FALSE)
plotGeoSituation(geo, main="random shortest path problem", sub = paste("iteration:", 
	geo$shortestpath$iter))
  
 # 