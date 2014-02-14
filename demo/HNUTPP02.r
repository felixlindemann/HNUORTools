
# Setup is taken from: WS13/14 HNU OR-Tutorium
# Working.Paper 2 Case Study 2 
  geo<-HNUGeoSituation.create()
  geo<-add(geo,new("HNUWarehouse", id="L1", x=25,   y=70,   supply = 350   ))
  geo<-add(geo,new("HNUWarehouse", id="L2", x=150,  y=115,  supply = 450   ))
  geo<-add(geo,new("HNUWarehouse", id="L3", x=80,   y=140,  supply = 300   ))
  geo<-add(geo,new("HNUWarehouse", id="L4", x=160,  y=10,   supply = 120   ))


  geo<-add(geo,new("HNUCustomer",  id="K1", x=15,   y=130,  demand = 150   ))
  geo<-add(geo,new("HNUCustomer",  id="K2", x=60,   y=80,   demand = 300   ))
  geo<-add(geo,new("HNUCustomer",  id="K3", x=175,  y=140,  demand = 180   ))
  geo<-add(geo,new("HNUCustomer",  id="K4", x=50,   y=100,  demand = 120   ))
  geo<-add(geo,new("HNUCustomer",  id="K5", x=30,   y=40,   demand = 100   ))
  geo<-add(geo,new("HNUCustomer",  id="K6", x=140,  y=80,   demand = 40    ))
  geo<-add(geo,new("HNUCustomer",  id="K7", x=100,  y=15,   demand = 80    ))
  geo<-add(geo,new("HNUCustomer",  id="K8", x=155,  y=55,   demand = 120   ))
  geo<-add(geo,new("HNUCustomer",  id="K9", x=125,  y=145,  demand = 130   ))
 
 # solve with column-mimimum-method use default domschke.version = 2007  
  geo<- HNU.OR.TPP.CMM(geo)   
  x <- geo$transportplan # store transportplan in local variable
 
   x[1,1] == 150   # TRUE
   x[3,1] == 0     # TRUE
   x[3,6] == 40    # TRUE
   x[3,7] == 80    # TRUE
   x[4,9] == 120   # TRUE
  
  col<- 2:5
  plotGeoSituation(geo, main="Tutorium #2 WS 13/14 - Aufg. 2",warehouses.bg.col = col, zoom=1.7, plotNodes=FALSE,plotGrid=FALSE)
  plotGeoSituation.transportplan(geo, arrow.point.cex = 4, arrow.point.bg = col)


 # solve with column-mimimum-method use domschke.version = 1995 
  geo<- HNU.OR.TPP.CMM(geo, domschke.version = 1995)   

  x <- geo$transportplan # store transportplan in local variable
 

   x[1,1] == 150   # TRUE
   x[3,1] == 0     # TRUE
   x[3,6] == 0    # TRUE
   x[3,5] == 100    # TRUE
   x[4,9] == 0     # TRUE
  