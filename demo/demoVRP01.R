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

alpha <- 0
geo <- VRP.SWEEP(geo, alpha = alpha, constraint.cap = 50, digits=0)
vrp <- geo$warehouses$vrp[[1]]

vrp$x # print x-Matrix
vrp$cij # print cij-Matrix 
vrp$F == sum(vrp$x * vrp$cij)

plot(
  geo, 
  main="Vehicle Routing Problem (VRP)",
  sub = paste("F:", vrp$F),
  font.cex=0.75, 
  zoom = 2, 
  drawVRP=TRUE
)

if(alpha <0) alpha <- alpha + 2*pi

phi <- seq(alpha, alpha + 0.5*pi , length=100) 
r <- 50

x <- geo$warehouses$x[1] + r * cos(phi)
y <- geo$warehouses$x[1] + r * sin(phi) 

lines(x,y, col='red', lty=2)
arrows(x[99], y[99], x[100], y[100], code  = 2, length  = 0.15, col='red') 

lines(c(geo$warehouses$x[1], x[1]),c(geo$warehouses$y[1], y[1]), col='red', lty=2)
# lines(c(geo$warehouses$x[1], x[100]),c(geo$warehouses$y[1], y[100]), col='red', lty=2)
