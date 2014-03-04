
# Setup is taken from: SoSe13 HNU OR-Tutorium
# Working.Paper 5 Case Study 2 
  geo<-HNUGeoSituation.create()
  geo<-add(geo,new("HNUWarehouse", id="W1", x=31,   y=147,   fixcosts = 52093   ))
  geo<-add(geo,new("HNUWarehouse", id="W2", x=-91,  y=95,  fixcosts = 62923   ))
  geo<-add(geo,new("HNUWarehouse", id="W3", x=140,   y=-74,  fixcosts = 60182   ))
  geo<-add(geo,new("HNUWarehouse", id="W4", x=45,  y=56,   fixcosts = 64133   ))
  geo<-add(geo,new("HNUWarehouse", id="W5", x=-40,  y=99,   fixcosts = 67246   ))


  geo<-add(geo,new("HNUCustomer",  id="K1", x=97,   y=43,  demand = 106   ))
  geo<-add(geo,new("HNUCustomer",  id="K2", x=63,   y=76,   demand = 126   ))
  geo<-add(geo,new("HNUCustomer",  id="K3", x=140,  y=-122,  demand = 264   ))
  geo<-add(geo,new("HNUCustomer",  id="K4", x=-126,   y=-58,  demand = 279   ))
  geo<-add(geo,new("HNUCustomer",  id="K5", x=-134,   y=148,   demand = 119   ))
  geo<-add(geo,new("HNUCustomer",  id="K6", x=22,  y=-114,   demand = 239    ))
  geo<-add(geo,new("HNUCustomer",  id="K7", x=-34,  y=8,   demand = 203    ))
  geo<-add(geo,new("HNUCustomer",  id="K8", x=3,  y=-138,   demand = 189   ))
  geo<-add(geo,new("HNUCustomer",  id="K9", x=-40,  y=132,  demand = 143   ))
  geo<-add(geo,new("HNUCustomer",  id="K10", x=-85,  y=-130,  demand = 164   ))
 

 dij <- round(HNU.OR.getDistanceMatrix(geo, "warehouses", "customers"))
 
 demand <- sapply(geo$customers, function(o){o$demand})

 cij <- t(t(dij)*demand)  * 2 

 
 geo<-HNU.OR.WLP.ADD(geo, cij = cij)



y<-geo$wlp.solution$y
  col<- y * 2:(length(y)+1)
  plotGeoSituation(geo, main="Tutorium #5 SoSe 13 - Aufg. 2",warehouses.bg.col = col, zoom=3, 
    plotNodes=FALSE,plotGrid=FALSE)
  plotGeoSituation.transportplan(geo, isWLP=TRUE, arrow.point.cex = 4, arrow.bg.col = col)
  title(sub=paste("Totalcosts: ",round(r$wlp.solution$totalcosts) ))
