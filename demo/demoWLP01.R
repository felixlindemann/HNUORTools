
# Setup is taken from: SoSe13  OR-Tutorium
# Working.Paper 5 Case Study 2 
geo<-new("GeoSituation")
geo<-add(geo,new("Warehouse", id="W1", x=31,   y=147,   fixcosts = 52093   ))
geo<-add(geo,new("Warehouse", id="W2", x=-91,  y=95,  fixcosts = 62923   ))
geo<-add(geo,new("Warehouse", id="W3", x=140,   y=-74,  fixcosts = 60182   ))
geo<-add(geo,new("Warehouse", id="W4", x=45,  y=56,   fixcosts = 64133   ))
geo<-add(geo,new("Warehouse", id="W5", x=-40,  y=99,   fixcosts = 67246   ))


geo<-add(geo,new("Customer",  id="K1", x=97,   y=43,  demand = 106   ))
geo<-add(geo,new("Customer",  id="K2", x=63,   y=76,   demand = 126   ))
geo<-add(geo,new("Customer",  id="K3", x=140,  y=-122,  demand = 264   ))
geo<-add(geo,new("Customer",  id="K4", x=-126,   y=-58,  demand = 279   ))
geo<-add(geo,new("Customer",  id="K5", x=-134,   y=148,   demand = 119   ))
geo<-add(geo,new("Customer",  id="K6", x=22,  y=-114,   demand = 239    ))
geo<-add(geo,new("Customer",  id="K7", x=-34,  y=8,   demand = 203    ))
geo<-add(geo,new("Customer",  id="K8", x=3,  y=-138,   demand = 189   ))
geo<-add(geo,new("Customer",  id="K9", x=-40,  y=132,  demand = 143   ))
geo<-add(geo,new("Customer",  id="K10", x=-85,  y=-130,  demand = 164   ))

#getDistanceMatrix
dij <- getDistanceMatrix(geo, "warehouses", "customers", digits=0)

#Calculate Cij
demand <-geo$customers$demand
cij <- t(t(dij)*demand)  * 2 

#Solve using Add
geo<-WLP.ADD(geo, cij = cij)

#get Solution
y<-geo$wlp$y

#colorize Solution
col<- y * 2:(length(y)+1)

plotGeoSituation(
  geo, 
  main="Tutorium #5 SoSe 13 - Aufg. 2",
  sub=paste("Totalcosts: ",round(geo$wlp$F)),
  warehouse.bg.col = col, 
  zoom=3, 
  plotNodes=FALSE,
  plotGrid=FALSE, 
  drawWLP=TRUE, 
  arrow.point.cex = 4, 
  arrow.bg.col = col
)

