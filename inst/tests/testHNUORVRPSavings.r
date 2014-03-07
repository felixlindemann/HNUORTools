# Das Erkennen von zusammengesetzten Spalten- und Zeilennamen 
# führt gelegentlich zu Problemen wenn diese zusammengesetzt [paste(...)] wurden
# Daher ist ist es erforderlich, Strings nicht als Factors zu setzen.
options(stringsAsFactors=FALSE) #WICHTIG!!

context("Testing  VRP-Savings-Algorithm")  
 
context("\tTest 01: is solution for a VRP calculated with the Savings-Algorithm correctly?") 
test_that("Test for VRP- Savings-Algorithm", { 

  # Setup for: SoSe13  OR-Tutorium
  # Working.Paper 9 Case Study    
  geo<-new("GeoSituation")   
  geo<-add(geo,new("Customer",  id="K1",  x=-129.19173,   y=-139.83131, demand = 14))
  geo<-add(geo,new("Customer",  id="K2",  x=  95.33256,   y=- 96.36450, demand = 18))
  geo<-add(geo,new("Customer",  id="K3",  x= 132.78652,   y=  42.49961, demand = 14))
  geo<-add(geo,new("Customer",  id="K4",  x=- 69.18544,   y=-143.13668, demand = 14))
  geo<-add(geo,new("Customer",  id="K5",  x=- 99.19556,   y=-147.50255, demand = 13))

  
  geo<-add(geo,new("Warehouse", id="W1",  x=0,   y=0, 
      supply = sum(sapply(geo$customers, function(o){o$demand}))))
  
  expect_true(geo$warehouses[[1]]$supply == (3*14+18+13))
  
  #Calculate Savings
  # what is expected:
  #     Calculate TPP.x
  #     Calculate TSP.cij 
  #     Calculate VRP.savings
  #         2 tours
  #         F = 1825
  #         
  geo<-  VRP.SAVINGS(geo, costfactor=2.2, 
          round.cij=TRUE, alpha=1.2 , 
          log=FALSE,
          vehiclecapacity.maxstops =100, # avoid warning.
          vehiclecapacity = 50)
  cij <- geo$tsp.costs
  expect_true(geo$tpp.x[1,1] == 14)
  expect_true(geo$tpp.x[1,3] == 14)
  expect_true(geo$tpp.x[1,5] == 13)
 
  expect_true(cij[1,3] == 298)
  expect_true(cij[1,3] == cij[3,1])

  expect_true(cij[3,5] == 376)
  expect_true(cij[5,3] == cij[3,5])

  w <- geo$warehouses[[1]]
  vrp<-w$vrp
  touren <- vrp$tours
  x<- vrp$x

  #check basic results
  expect_true(vrp$totalcosts == 1825)
  expect_true(vrp$totalcosts == sum(cij*x))

  expect_true(vrp$maxcapacity == 50)
  expect_true(length(touren) == 2)

  #check tour 1 in detail
  tour1 <- touren[[1]]
  indices <- tour1$stops.indices
  
  expect_true(tour1$costs == 904)
  expect_true(tour1$loading == 41)
  expect_true(tour1$stops == 3)
  expect_true(tour1$stops == length(tour1$stops.indices))
  expect_true(tour1$stops == length(tour1$stops.list))


  #check correct routing
  expect_true(indices[[1]] == 4)
  expect_true(indices[[2]] == 5)
  expect_true(indices[[3]] == 1) 

  # check adjascens-matrix
  expect_true(x[1,indices[[1]]+1] ==1)
  expect_true(x[indices[[1]]+1,indices[[2]]+1] ==1)
  expect_true(x[indices[[2]]+1,indices[[3]]+1] ==1) 
  expect_true(x[indices[[3]]+1,1] ==1)
  # is equal to
  expect_true(x[1,5] ==1)
  expect_true(x[5,6] ==1)
  expect_true(x[6,2] ==1)
  expect_true(x[2,1] ==1)


  tour2 <- touren[[2]]
  indices <- tour2$stops.indices
  
  expect_true(tour2$costs == 921)
  expect_true(tour2$loading == 32)
  expect_true(tour2$stops == 2)
  expect_true(tour2$stops == length(tour2$stops.indices))
  expect_true(tour2$stops == length(tour2$stops.list))

  expect_true(indices[[1]] == 2)
  expect_true(indices[[2]] == 3) 
  
  expect_true(x[1,indices[[1]]+1] ==1)
  expect_true(x[indices[[1]]+1,indices[[2]]+1] ==1)
  expect_true(x[indices[[2]]+1,1] ==1)

} 
) 
context("done.")   
context("--------------------------------------------------")  


