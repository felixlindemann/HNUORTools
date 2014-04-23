context("Testing  GeoSituation")  

#create 4 Nodes

#  GeoSituation

context("\tTest 01: can objects be added correctly?") 
test_that("Test for adding objects to geoSituation", {
  geo<-new("GeoSituation") 
	geo<-add(geo,new("Node"))
  	expect_true(length(geo$nodes) == 1)
	geo<-add(geo,new("Node"))
  	expect_true(length(geo$nodes) == 2)
	geo<-add(geo,new("Warehouse"))
  	expect_true(length(geo$nodes) == 2)
  	expect_true(length(geo$warehouses) == 1)
	geo<-add(geo,new("Warehouse"))
  	expect_true(length(geo$nodes) == 2)
  	expect_true(length(geo$warehouses) == 2)
	geo<-add(geo,new("Customer"))
  	expect_true(length(geo$nodes) == 2)
  	expect_true(length(geo$warehouses) == 2)
  	expect_true(length(geo$customers) == 1)
	geo<-add(geo,new("Customer"))
  	expect_true(length(geo$nodes) == 2)
  	expect_true(length(geo$warehouses) == 2)
  	expect_true(length(geo$customers) == 2)

}) 

context("\tTest 02: can the distance matrix be created correctly?") 
test_that("Test for calculation of distances", {
  
  #szenario taken from Tutorium #2 ws 13/14 aufg. 2
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
  
  m<-getDistanceMatrix(geo,"Warehouse", "Customer", digits=0)
  
  expect_true(length(geo$warehouses) ==4)
  expect_true(length(geo$customers) ==9)
  
  expect_true(nrow(m) ==4)
  expect_true(ncol(m) ==9)
  
  expect_true(m[1,1] == 61)
  expect_true(m[1,2] == 36)
  expect_true(m[3,4] == 50)
  expect_true(m[4,6] == 73)
  expect_true(m[2,4] + m[3,5] + m[4,2] == 335)
  
})
context("done.")   
context("--------------------------------------------------")  

context("\tTest 03: can the distance matrix be created correctly with a cost factor?") 
test_that("Test for calculation of distances with calc", {
  
  #szenario taken from Tutorium #2 ws 13/14 aufg. 2
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
  
  m<-getDistanceMatrix(geo,"Warehouse", "Customer", digits=0, costfactor=2)
  expect_true(length(geo$warehouses) ==4)
  expect_true(length(geo$customers) ==9)
  
  expect_true(nrow(m) ==4)
  expect_true(ncol(m) ==9)
  
  expect_true(m[1,1] == 122)
  expect_true(m[1,2] == 72)
  expect_true(m[3,4] == 100)
  expect_true(m[4,6] == 146)
  expect_true(m[2,4] + m[3,5] + m[4,2] == 335*2)
  
})
context("done.")   
context("--------------------------------------------------")  



