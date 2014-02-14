 
#create 4 Nodes
 
geo<-HNUGeoSituation.create()

n1<- HNUNode.create(id = "n1", x=10, y=20)
n2<- HNUNode.create(id = "n2",x=13, y=24)
n3<- new("HNUNode", id = "n3",x=7, y=16)
n4<- new("HNUNode", id = "n4",x=7, y=20)


geo<-add(geo,n1)
geo<-add(geo,n2)
geo<-add(geo,n3)
geo<-add(geo,n4)


l1 <- HNULink.create(n1,n2)
l2 <- HNULink.create(n1,n3)
l3 <- HNULink.create(n1,n4)
l4 <- HNULink.create(n3,n4)

geo<-add(geo,l1)
geo<-add(geo,l2)
geo<-add(geo,l3)
geo<-add(geo,l4)

 
  
geo <- HNU.OR.SPP.Dijkstra(geo,start=3, log=FALSE, plot=TRUE, main="random shortest path problem")
