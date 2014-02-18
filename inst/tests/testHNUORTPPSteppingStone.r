context("Testing HNU OR TPP Stepping Stone")  
 	
context("\tTest 01: is procedure 'Stepping Stone' working correctly?") 
test_that("'Stepping Stone' works correctly", {
 	geo<-HNUGeoSituation.create()
	geo<-add(geo,new("HNUWarehouse", id="L1", x=25,   y=70,   supply = 350   ))
	geo<-add(geo,new("HNUWarehouse", id="L2", x=150,  y=115,  supply = 450   ))
	geo<-add(geo,new("HNUWarehouse", id="L3", x=80,   y=140,  supply = 340   ))
	geo<-add(geo,new("HNUWarehouse", id="L4", x=160,  y=10,   supply = 120   ))
 
	geo<-add(geo,new("HNUCustomer",  id="K1", x=15,   y=130,  demand = 150   ))
	geo<-add(geo,new("HNUCustomer",  id="K2", x=60,   y=80,   demand = 300   ))
	geo<-add(geo,new("HNUCustomer",  id="K3", x=175,  y=140,  demand = 180   ))
	geo<-add(geo,new("HNUCustomer",  id="K4", x=50,   y=100,  demand = 120   ))
	geo<-add(geo,new("HNUCustomer",  id="K5", x=30,   y=40,   demand = 100   ))
	geo<-add(geo,new("HNUCustomer",  id="K6", x=140,  y=80,   demand = 40    ))
	geo<-add(geo,new("HNUCustomer",  id="K7", x=100,  y=15,   demand = 80    ))
	geo<-add(geo,new("HNUCustomer",  id="K8", x=155,  y=55,   demand = 160   )) 
	geo<-add(geo,new("HNUCustomer",  id="K9", x=125,  y=145,  demand = 130   ))


	# NO changes here  
 
	geo<- HNU.OR.TPP.MMM(geo)  
	x <- geo$tpp.x
	cij<- geo$tpp.costs
	totalcosts <- round(sum(x*cij))
	expect_equal(totalcosts,   61147 )# 
	 

	geo<- HNU.OR.TPP.SteppingStone(geo, log=FALSE) 	
 	x <- geo$tpp.x
 	totalcosts <- round(sum(x*cij))# 
	expect_equal(totalcosts,    58042 )# 
	
 	demand <- sapply(geo$customers, function(o){o$demand})
 	supply <- sapply(geo$warehouses, function(o){o$supply})
 	
 	expect_true(sum(x) == sum(demand) )# true
 	expect_true(sum(x) == sum(supply) )# true


	expect_true(x[1,1] == 0 )# true
	expect_true(x[3,1] == 150 )# true
	expect_true(x[3,6] == 0 )# true
 	expect_true(x[3,4] == 120 )# true
 	expect_true(x[4,7] == 80 )# true
 
  
}) 

context("done.")   
context("--------------------------------------------------")  
