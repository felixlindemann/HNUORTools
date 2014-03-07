	geo<-new("GeoSituation")
 	#example taken from
 	#Bloech Management Methoden und Optimalplanung S. 72
	geo<-add(geo,new("Warehouse", id="L1", x=25,   y=70,   supply = 100   ))
	geo<-add(geo,new("Warehouse", id="L2", x=150,  y=115,  supply = 130   ))
	geo<-add(geo,new("Warehouse", id="L3", x=80,   y=140,  supply = 170   )) 
 
	geo<-add(geo,new("Customer",  id="K1", x=15,   y=130,  demand = 150   ))
	geo<-add(geo,new("Customer",  id="K2", x=60,   y=80,   demand = 120   ))
	geo<-add(geo,new("Customer",  id="K3", x=175,  y=140,  demand = 80   ))
	geo<-add(geo,new("Customer",  id="K4", x=50,   y=100,  demand = 50   ))
	  

	# NO changes here   
	geo<- TPP.MMM(geo)  # just for setting up all variables.
	x <- geo$tpp.x * 0 
	# setting values from example
	x[1,1] <- 100
	x[2,1] <- 50
	x[2,2] <- 30
	x[2,4] <- 50
	x[3,2] <- 90
	x[3,3] <- 80
	geo$tpp.x <- x 
	geo$tpp.costs <- matrix(c(3,5,7,11,1,4,6,3,5,8,12,7), ncol=4, byrow=TRUE)
 	cij <- geo$tpp.costs

 	demand <- sapply(geo$customers, function(o){o$demand})
 	supply <- sapply(geo$warehouses, function(o){o$supply})
 	
 	sum(x) == sum(demand) # true
 	sum(x) == sum(supply) # true

 	totalcosts <- round(sum(x*cij))# 
	totalcosts == 2300 # true 

	geo<- TPP.MODI(geo, log=TRUE) 	
 	x <- geo$tpp.x
 	totalcosts <- round(sum(x*cij))# 
	totalcosts == 2040 # true 
	


	x[1,2] == 20 # true
	x[2,1] == 80 # true
	x[3,1] == 70 # true
 	x[1,3] == 80 # true
 	x[2,4] == 50 # true
	x[3,2] == 100 # true
 