
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

  m<-getDistanceMatrix(geo,"warehouse", "customer")
  m<-round(m)

  length(geo$warehouses) ==4  # TRUE
  length(geo$customers) ==9 # TRUE

  nrow(m) ==4 # TRUE
  ncol(m) ==9 # TRUE

  m[1,1] == 61  # TRUE
  m[1,2] == 36  # TRUE
  m[3,4] == 50  # TRUE
  m[4,6] == 73  # TRUE
  m[2,4] + m[3,5] + m[4,2] == 335 # TRUE

  m

# demo(HNUTPP01)

  plotGeoSituation(geo, main="Tutorium #2 WS 13/14 - Aufg. 2", zoom=1.7, plotNodes=FALSE,plotGrid=FALSE)
