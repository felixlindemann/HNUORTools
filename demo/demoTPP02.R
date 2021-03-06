# Setup is taken from: WS13/14  OR-Tutorium
# Working.Paper 2 Case Study 2 
geo<-new("GeoSituation")
geo<-add(geo,new("Warehouse", id="L1", x=25,   y=70,   supply = 350   ))
geo<-add(geo,new("Warehouse", id="L2", x=150,  y=115,  supply = 450   ))
geo<-add(geo,new("Warehouse", id="L3", x=80,   y=140,  supply = 300   ))
geo<-add(geo,new("Warehouse", id="L4", x=160,  y=10,   supply = 120   ))


geo<-add(geo,new("Customer",  id="K1", x=15,   y=130,  demand = 150   ))
geo<-add(geo,new("Customer",  id="K2", x=60,   y=80,   demand = 300   ))
geo<-add(geo,new("Customer",  id="K3", x=175,  y=140,  demand = 180   ))
geo<-add(geo,new("Customer",  id="K4", x=50,   y=100,  demand = 120   ))
geo<-add(geo,new("Customer",  id="K5", x=30,   y=40,   demand = 100   ))
geo<-add(geo,new("Customer",  id="K6", x=140,  y=80,   demand = 40    ))
geo<-add(geo,new("Customer",  id="K7", x=100,  y=15,   demand = 80    ))
geo<-add(geo,new("Customer",  id="K8", x=155,  y=55,   demand = 120   ))
geo<-add(geo,new("Customer",  id="K9", x=125,  y=145,  demand = 130   ))

# solve with column-mimimum-method 
geo<- TPP.CMM(geo)   
x <- geo$tpp$x # store transportplan in local variable

x[1,1] == 150   # TRUE
x[3,1] == 0     # TRUE
x[3,6] == 0    # TRUE
x[3,5] == 100    # TRUE
x[4,9] == 0     # TRUE

col<- 2:5
plotGeoSituation(geo, main="Tutorium #2 WS 13/14 - Aufg. 2",
     warehouse.bg.col = col, 
     zoom=1.7, 
     plotNodes=FALSE,
     plotGrid=FALSE,
     arrow.point.cex = 4, 
     arrow.point.bg = col,
     drawTPP = TRUE
)
 