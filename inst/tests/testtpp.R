options(stringsAsFactors=FALSE) #WICHTIG!!

context("Testint Transportation Problem") 
context("\tTest 01: Does TPP.Prepare work correctly?") 
test_that("... a Dummy Customer is added.", { 
  geo<- new("GeoSituation")
  geo<- add(geo, new("Customer", id="K1", demand = 20))
  geo<- add(geo, new("Customer", id="K2", demand = 10))
  geo<- add(geo, new("Customer", id="K3", demand = 20))
  geo<- add(geo, new("Customer", id="K4", demand = 50))
  
  geo<- add(geo, new("Warehouse", id="W1", supply = 50))
  geo<- add(geo, new("Warehouse", id="W2", supply = 30))
  geo<- add(geo, new("Warehouse", id="W3", supply = 30))
  
  
  geo<- TPP.Prepare(geo) 
   
  expect_true(length(geo$customers) == 5)
  expect_true(geo$customers$demand[5] == 10)
  
}) 
test_that("... a Dummy Warehouse is added.", { 
  geo<- new("GeoSituation")
  geo<- add(geo, new("Customer", id="K1", demand = 20))
  geo<- add(geo, new("Customer", id="K2", demand = 10))
  geo<- add(geo, new("Customer", id="K3", demand = 30))
  geo<- add(geo, new("Customer", id="K4", demand = 60))
  
  geo<- add(geo, new("Warehouse", id="W1", supply = 50))
  geo<- add(geo, new("Warehouse", id="W2", supply = 30))
  geo<- add(geo, new("Warehouse", id="W3", supply = 20))
  
   
  geo<- TPP.Prepare(geo)
  expect_true(length(geo$warehouses) == 4)
  expect_true(geo$warehouses$supply[4] == 20)
  
}) 

context("\tTest 02: Does TPP.NorthWestCorner-Rule work correctly?") 
test_that("Distances are calculated as expected.", { 
  geo<- new("GeoSituation")
  geo<- add(geo, new("Customer", id="K1", demand = 20))
  geo<- add(geo, new("Customer", id="K2", demand = 20))
  geo<- add(geo, new("Customer", id="K3", demand = 20))
  geo<- add(geo, new("Customer", id="K4", demand = 50))
  
  geo<- add(geo, new("Warehouse", id="W1", supply = 50))
  geo<- add(geo, new("Warehouse", id="W2", supply = 30))
  geo<- add(geo, new("Warehouse", id="W3", supply = 30))
   
  geo<- TPP.NW(geo)
  
  
  expect_true(geo$tpp$x[1,1]==  20)
  expect_true(geo$tpp$x[1,2]==  20)
  expect_true(geo$tpp$x[1,3]==  10)
  expect_true(geo$tpp$x[2,3]==  10) 
  expect_true(geo$tpp$x[2,4]==  20)
  expect_true(geo$tpp$x[3,4]==  30)
  
}) 
context("\tTest 03: Does TPP.Column-Minimum work correctly?") 
test_that("Transportation Plan is calculated as expected.", { 
  set.seed(1)
  geo<- new("GeoSituation")
  geo<- add(geo, new("Customer", id="K1", demand = 20))
  geo<- add(geo, new("Customer", id="K2", demand = 20))
  geo<- add(geo, new("Customer", id="K3", demand = 20))
  geo<- add(geo, new("Customer", id="K4", demand = 50))
  
  geo<- add(geo, new("Warehouse", id="W1", supply = 50))
  geo<- add(geo, new("Warehouse", id="W2", supply = 30))
  geo<- add(geo, new("Warehouse", id="W3", supply = 30))
  
  geo<- TPP.CMM(geo)
  geo$tpp$x
  
  expect_true(geo$tpp$x[1,1]==  20)
  expect_true(geo$tpp$x[1,2]==  20)
  expect_true(geo$tpp$x[3,3]==  20)
  expect_true(geo$tpp$x[2,4]==  30) 
  expect_true(geo$tpp$x[1,4]==  10)
  expect_true(geo$tpp$x[3,4]==  10)
  
})  
context("\tTest 04: Does TPP.Matrix-Minimum work correctly?") 
test_that("Transportation Plan is calculated as expected.", { 
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
  
  
  # NO changes here  
  
  geo<- TPP.MMM(geo)  
  x <- geo$tpp$x
  cij<- geo$tpp$cij
  
  totalcosts <- round(sum(x*cij))
  expect_true(totalcosts == 61147 )# 
  
  geo<- TPP.SteppingStone(geo, log=FALSE) 	
  x <- geo$tpp$x
  totalcosts <- round(sum(x*cij))#  
  expect_true(totalcosts == 58042 )# 
  
  demand <- geo$customers$demand
  supply <- geo$warehouses$supply
  
  expect_true(sum(x) == sum(demand) )# true
  expect_true(sum(x) == sum(supply) )# true
  
  expect_true(x[1,1] == 0 )# true
  expect_true(x[3,1] == 150 )# true
  expect_true(x[3,6] == 0 )# true
  expect_true(x[3,4] == 120 )# true 
  expect_true(x[4,7] == 80 )# true
  
  
}) 

context("\tTest 05: Does TPP.Stepping working correctly - does it produce the same results as in literature?") 
test_that("'Stepping-Stone-Method' works correctly", {
  geo<-new("GeoSituation")
  #example taken from
  #Bloech Management Methoden und Optimalplanung S. 72
  # coordinates of Locations can be random
  geo<-add(geo,new("Warehouse", id="L1", x=25,   y=70,   supply = 100   ))
  geo<-add(geo,new("Warehouse", id="L2", x=150,  y=115,  supply = 130   ))
  geo<-add(geo,new("Warehouse", id="L3", x=80,   y=140,  supply = 170   )) 
  
  geo<-add(geo,new("Customer",  id="K1", x=15,   y=130,  demand = 150   ))
  geo<-add(geo,new("Customer",  id="K2", x=60,   y=80,   demand = 120   ))
  geo<-add(geo,new("Customer",  id="K3", x=175,  y=140,  demand = 80   ))
  geo<-add(geo,new("Customer",  id="K4", x=50,   y=100,  demand = 50   ))
  
  demand <- geo$customers$demand
  supply <- geo$warehouses$supply
  M<- length(geo$warehouses)
  N<- length(geo$customers) 
  countBasisVariables <- M+N-1
  
  expect_true( sum(demand)  == sum(supply) )# true
  x<-getInitialMatrix(geo, initialvalue=0) # just for setting up all variables.
  
  # setting values from example
  x[1,1] <- 100
  x[2,1] <- 50
  x[2,2] <- 30
  x[2,4] <- 50
  x[3,2] <- 90
  x[3,3] <- 80
  geo$tpp$x <- x 
  cij <- matrix(c(3,5,7,11,1,4,6,3,5,8,12,7), ncol=4, byrow=TRUE) 
  geo$tpp$cij <- cij
  
  totalcosts <- round(sum(x*cij)) 
  expect_equal(totalcosts, 2300 )# 
  
  geo<- TPP.SteppingStone(geo,  log=FALSE)   
  opp <- geo$tpp$oppcosts
  expect_true(sum(is.na(opp)) == M*N- countBasisVariables)
  x <- geo$tpp$x 
  
  totalcosts <- round(sum(x*cij))# 
  expect_equal(totalcosts,    2040 )# 
  
  expect_true(sum(x) == sum(demand) )# true
  expect_true(sum(x) == sum(supply) )# true
  
  
  expect_true(x[1,2] == 20 )# true
  expect_true(x[2,1] == 80 )# true
  expect_true(x[3,1] == 70 )# true
  expect_true(x[1,3] == 80 )# true
  expect_true(x[2,4] == 50 )# true
  expect_true(x[3,2] == 100 )# true
  
  
}) 
context("\tTest 06: Does TPP.MODI working correctly 1 - does it produce the same results as in literature?") 
test_that("'MODI-Method' works correctly", {
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
  
  
  # NO changes here  
  
  geo<- TPP.MMM(geo)  
  x <- geo$tpp$x
  cij<- geo$tpp$cij
  totalcosts <- round(sum(x*cij))
  expect_equal(totalcosts,   61147 )# 
  
  
  geo<- TPP.MODI(geo, log=FALSE)   
  x <- geo$tpp$x
  totalcosts <- round(sum(x*cij))# 
  expect_equal(totalcosts,    58042 )# 
  
  demand <- geo$customers$demand
  supply <- geo$warehouses$supply
  
  expect_true(sum(x) == sum(demand) )# true
  expect_true(sum(x) == sum(supply) )# true
  
  
  expect_true(x[1,1] == 0 )# true
  expect_true(x[3,1] == 150 )# true
  expect_true(x[3,6] == 0 )# true
  expect_true(x[3,4] == 120 )# true
  expect_true(x[4,7] == 80 )# true
  
  
}) 
context("\tTest 07: Does TPP.MODI working correctly 2 - does it produce the same results as in literature?") 
test_that("'MODI-Method' works correctly", {
  geo<-new("GeoSituation")
  #example taken from
  #Bloech Management Methoden und Optimalplanung S. 72
  # coordinates of Locations can be random
  geo<-add(geo,new("Warehouse", id="L1", x=25,   y=70,   supply = 100   ))
  geo<-add(geo,new("Warehouse", id="L2", x=150,  y=115,  supply = 130   ))
  geo<-add(geo,new("Warehouse", id="L3", x=80,   y=140,  supply = 170   )) 
  
  geo<-add(geo,new("Customer",  id="K1", x=15,   y=130,  demand = 150   ))
  geo<-add(geo,new("Customer",  id="K2", x=60,   y=80,   demand = 120   ))
  geo<-add(geo,new("Customer",  id="K3", x=175,  y=140,  demand = 80   ))
  geo<-add(geo,new("Customer",  id="K4", x=50,   y=100,  demand = 50   ))
  
  demand <- geo$customers$demand
  supply <- geo$warehouses$supply
  expect_true( sum(demand)  == sum(supply) )# true
  x<-getInitialMatrix(geo, initialvalue=0) # just for setting up all variables.
  
  # setting values from example
  x[1,1] <- 100
  x[2,1] <- 50
  x[2,2] <- 30
  x[2,4] <- 50
  x[3,2] <- 90
  x[3,3] <- 80
  geo$tpp$x <- x 
  cij <- matrix(c(3,5,7,11,1,4,6,3,5,8,12,7), ncol=4, byrow=TRUE) 
  geo$tpp$cij <- cij
  
  
  geo<- TPP.MODI(geo,  log=FALSE) 	 
  x <- geo$tpp$x
  totalcosts <- round(sum(x*cij))# 
  expect_equal(totalcosts,    2040 )# 
  
  expect_true(sum(x) == sum(demand) )# true
  expect_true(sum(x) == sum(supply) )# true
  
  
  expect_true(x[1,2] == 20 )# true
  expect_true(x[2,1] == 80 )# true
  expect_true(x[3,1] == 70 )# true
  expect_true(x[1,3] == 80 )# true
  expect_true(x[2,4] == 50 )# true
  expect_true(x[3,2] == 100 )# true
  
  
}) 

context("done.")   
context("--------------------------------------------------")  
