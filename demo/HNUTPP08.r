	geo<-HNUGeoSituation.create()
 	#example taken from
 	#Bloech Management Methoden und Optimalplanung S. 72
 	# coordinates of Locations can be random
	geo<-add(geo,new("HNUWarehouse", id="L1", x=25,   y=70,   supply = 100   ))
	geo<-add(geo,new("HNUWarehouse", id="L2", x=150,  y=115,  supply = 130   ))
	geo<-add(geo,new("HNUWarehouse", id="L3", x=80,   y=140,  supply = 170   )) 
 
	geo<-add(geo,new("HNUCustomer",  id="K1", x=15,   y=130,  demand = 150   ))
	geo<-add(geo,new("HNUCustomer",  id="K2", x=60,   y=80,   demand = 120   ))
	geo<-add(geo,new("HNUCustomer",  id="K3", x=175,  y=140,  demand = 80   ))
	geo<-add(geo,new("HNUCustomer",  id="K4", x=50,   y=100,  demand = 50   ))
	  
 	demand <- sapply(geo$customers, function(o){o$demand})
 	supply <- sapply(geo$warehouses, function(o){o$supply})
 	sum(demand)  == sum(supply) # true
	x<-HNU.OR.getInitialMatrix(geo, initialvalue=0) # just for setting up all variables.
	
	# setting values from example
	x[1,1] <- 100
	x[2,1] <- 50
	x[2,2] <- 30
	x[2,4] <- 50
	x[3,2] <- 90
	x[3,3] <- 80
	geo$tpp.x <- x 
	cij <- matrix(c(3,5,7,11,1,4,6,3,5,8,12,7), ncol=4, byrow=TRUE) 
	geo$tpp.costs <- cij
	 
	totalcosts <- round(sum(x*cij))
 	cat(paste("\ntotalcosts: ", totalcosts, "\n"))
	totalcosts == 2300 # true
	   
	geo<- HNU.OR.TPP.SteppingStone(geo, log=TRUE) 	
 	opp <- geo$tpp.costs.opp 
	x <- geo$tpp.x 

 	totalcosts <- round(sum(x*cij))# 
	totalcosts ==    2040 # true
	
 	sum(x) == sum(demand) # true
 	sum(x) == sum(supply) # true


	x[1,2] == 20 # true
	x[2,1] == 80 # true
	x[3,1] == 70 # true
 	x[1,3] == 80 # true
 	x[2,4] == 50 # true
	x[3,2] == 100 # true
 