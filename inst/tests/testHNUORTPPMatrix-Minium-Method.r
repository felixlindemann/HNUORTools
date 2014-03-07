context("Testing  OR TPP Matrix-Minium-Method")  
 	
context("\tTest 01: is procedure 'Matrix-Minium-Method' working correctly?") 
test_that("'Matrix-Minium-Method' works correctly", {
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


	# NO changes here  
 
	geo<- TPP.MMM(geo)  
 	
 	x <- geo$tpp.x
 	
 	demand <- sapply(geo$customers, function(o){o$demand})
 	supply <- sapply(geo$warehouses, function(o){o$supply})
 	
 	expect_true(sum(x) == sum(demand))
 	expect_true(sum(x) == sum(supply))


	expect_true(x[1,1] == 0)
	expect_true(x[3,1] == 130)
	expect_true(x[3,6] == 0)
 	expect_true(x[3,4] == 120)
 	expect_true(x[4,8] == 120)
 
}) 

context("done.")   
context("--------------------------------------------------")  
