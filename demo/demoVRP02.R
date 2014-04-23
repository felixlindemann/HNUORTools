options(stringsAsFactors = FALSE) #important!!
s<-12
set.seed(s)
N<-5
bounds<-c(-150,150)
alpha<-3
demand.bounds<-c(10,20)

Werk<-data.frame( 
  id=paste("W",1:1,sep=""), 
  x= 0,
  y= 0
)

rownames(Werk)<-Werk$name

Kunden<-data.frame( 
  id=paste("K",1:N,sep=""), 
  x= runif(N,bounds[1],bounds[2]),
  y= runif(N,bounds[1],bounds[2])
) 
rownames(Kunden)<-Kunden$name 
locations<-rbind(Werk,Kunden)

# Add Demand
Kunden<-cbind(Kunden,demand=round(runif(N,demand.bounds[1],demand.bounds[2]),0))

geo<- new ("GeoSituation")

geo<-add(geo,new("Warehouse", Werk))
geo<-add(geo,new("Customer", Kunden))

geo <- VRP.SAVINGS(geo, alpha = 1.2, constraint.cap = 50, digits=0)
vrp <- geo$warehouses$vrp[[1]]

vrp$x # print x-Matrix
vrp$cij # print cij-Matrix
vrp$savings # print Savingstable
vrp$F == sum(vrp$x * vrp$cij)
plot(
  geo, 
  main="Vehicle Routing Problem (VRP)",
  sub = paste("F:", vrp$F),
  font.cex=0.75, 
  zoom = 2, 
  drawVRP=TRUE
)
