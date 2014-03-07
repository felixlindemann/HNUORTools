context("Testing  OR TPP Prepare")  
 	
context("\tTest 01: Will a Dummy-Warehouse be added?") 
test_that("Dummy Warehouse is added correctly.", {
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

	#changes here  
	geo<-add(geo,new("Customer",  id="K9", x=125,  y=145,  demand = 140   ))
 	
 	#test method for a result
	expect_warning(geo<-TPP.Prepare(geo), "Less supply than demand - dummy warehouse added.")
	 

	expect_true(length(geo$customers) == 9)
	expect_true(length(geo$warehouses) == 5)

	dummy <- geo$warehouses[[length(geo$warehouses)]]


	expect_true(dummy$id == "dummy")
	expect_true(dummy$supply == 10)
 
}) 

context("\tTest 02: Will a Dummy-Customer be added?") 
test_that("Dummy Customer is added correctly.", {

	geo<-new("GeoSituation")
	geo<-add(geo,new("Warehouse", id="L1", x=25,   y=70,   supply = 350   ))
	geo<-add(geo,new("Warehouse", id="L2", x=150,  y=115,  supply = 450   ))
	geo<-add(geo,new("Warehouse", id="L3", x=80,   y=140,  supply = 300   ))
 
	geo<-add(geo,new("Customer",  id="K1", x=15,   y=130,  demand = 150   ))
	geo<-add(geo,new("Customer",  id="K2", x=60,   y=80,   demand = 300   ))
	geo<-add(geo,new("Customer",  id="K3", x=175,  y=140,  demand = 180   ))
	geo<-add(geo,new("Customer",  id="K4", x=50,   y=100,  demand = 120   ))
	geo<-add(geo,new("Customer",  id="K5", x=30,   y=40,   demand = 100   ))
	geo<-add(geo,new("Customer",  id="K6", x=140,  y=80,   demand = 40    ))
	geo<-add(geo,new("Customer",  id="K7", x=100,  y=15,   demand = 80    ))
	geo<-add(geo,new("Customer",  id="K8", x=155,  y=55,   demand = 120   ))
	geo<-add(geo,new("Customer",  id="K9", x=125,  y=145,  demand = 130   ))


	geo<-add(geo,new("Warehouse", id="L4", x=160,  y=10,   supply = 130   ))
	

	expect_warning(geo<-TPP.Prepare(geo), "Less demand than supply - dummy customer added.")
	 
	expect_true(length(geo$customers) == 10)
	expect_true(length(geo$warehouses) == 4)
	dummy <- geo$customers[[length(geo$customers)]]


	expect_true(dummy$id == "dummy")
	expect_true(dummy$demand == 10)
 
}) 

context("\tTest 03: Nothing is added if not needed?") 
test_that("No Dummies are added.", {
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
 
	geo<- TPP.Prepare(geo)  

	expect_true(length(geo$warehouses) ==  4)
	expect_true(length(geo$customers)  ==  9 )
 
}) 

context("done.")   
context("--------------------------------------------------")  


