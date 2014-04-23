geo<-new("GeoSituation")
geo<-add(geo,new("Warehouse", id="L1", x=25,   y=70,   supply = 350   ))
geo<-add(geo,new("Warehouse", id="L2", x=150,  y=115,  supply = 450   ))
geo<-add(geo,new("Warehouse", id="L3", x=80,   y=140,  supply = 340   ))
geo<-add(geo,new("Warehouse", id="L4", x=160,  y=10,   supply = 120   ))

geo<-add(geo,new("Customer",  id="K1", x=15,   y=130,  demand = 150   ))
geo<-add(geo,new("Customer",  id="K2", x=60,   y=80,   demand = 300   ))
geo<-add(geo,new("Customer",  id="K3", x=175,  y=140,  demand = 180   ))
geo<-add(geo,new("Customer",  id="K4", x=50,   y=100,  demand = 120   ))
geo<-add(geo,new("Customer",  id="K5", x=30,   y=40,   demand = 100   ))
geo<-add(geo,new("Customer",  id="K6", x=140,  y=80,   demand = 40    ))
geo<-add(geo,new("Customer",  id="K7", x=100,  y=15,   demand = 80    ))
geo<-add(geo,new("Customer",  id="K8", x=155,  y=55,   demand = 160   )) 
geo<-add(geo,new("Customer",  id="K9", x=125,  y=145,  demand = 130   ))


# use Matrix-Minimum for initial Solution
geo<- TPP.MMM(geo)  
x <- geo$tpp$x
cij<- geo$tpp$cij
totalcosts <- sum(x*cij)# 61146.7 
cat("Total Costs of Matrix-Minimum Method: ",totalcosts,"\n")

#Optimize using Stepping Stone
geo<- TPP.SteppingStone(geo, log=TRUE)

x <- geo$tpp$x
totalcosts <- sum(x*cij)# 58042.3365659098
cat("Total Costs of optimal Solution (Stepping Stone): ",totalcosts,"\n")
 
x[1,1] == 0 # true
x[3,1] == 150 # true
x[3,6] == 0 # true
x[3,4] == 120 # true
x[4,7] == 80 # true

col<- 2:5
plot(
  geo, 
  main="Tutorium #2 WS 13/14 - Aufg. 2",
  warehouse.bg.col = col, 
  zoom=1.7, 
  plotNodes=FALSE,
  plotGrid=FALSE, 
  arrow.point.cex = 4, 
  arrow.point.bg = col,
  drawTPP = TRUE,
  sub=paste("Totalcosts: ",round(totalcosts) )
)
