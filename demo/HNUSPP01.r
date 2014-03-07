 
#create 4 Nodes
 
geo<-new("GeoSituation")

n1<- new("Node",id = "n1", x=10, y=20)
n2<- new("Node",id = "n2",x=13, y=24)
n3<- new("Node", id = "n3",x=7, y=16)
n4<- new("Node", id = "n4",x=7, y=20)


geo<-add(geo,n1)
geo<-add(geo,n2)
geo<-add(geo,n3)
geo<-add(geo,n4)


l1 <- new("Link",n1,n2)
l2 <- new("Link",n1,n3)
l3 <- new("Link",n1,n4)
l4 <- new("Link",n3,n4)

geo<-add(geo,l1)
geo<-add(geo,l2)
geo<-add(geo,l3)
geo<-add(geo,l4)

 
  
geo <- SPP.Dijkstra(geo,start=3, log=FALSE)
plotGeoSituation(geo, main="Demo shortest path problem", sub = paste("iteration:", geo$shortestpath$iter))