options(stringsAsFactors=FALSE) #WICHTIG!!
context("Testing  Vehicle Routing Problem ")  


context("\tTest 01: Does Sweep-Algorithm work properly?") 
test_that("Sweep works correctly", {  
  
  geo<-new("GeoSituation")
  #example taken from Domschke, Wolfgang (2010): Logistik. Rundreisen und Touren. 5., Aufl. Muenchen [u.a.]: Oldenbourg (2). S. 230
  geo<-add(geo, new("Warehouse", id="L", x=0, y=0, supply=10))
  geo<-add(geo, new("Customer", id="K01", x=4, y=1, demand = 1))
  geo<-add(geo, new("Customer", id="K02", x=1, y=2, demand = 1))
  geo<-add(geo, new("Customer", id="K03", x=0, y=5, demand = 1))
  geo<-add(geo, new("Customer", id="K04", x=-3, y=3, demand = 1))
  geo<-add(geo, new("Customer", id="K05", x=-2, y=1, demand = 1))
  geo<-add(geo, new("Customer", id="K06", x=-5, y=1, demand = 1))
  geo<-add(geo, new("Customer", id="K07", x=-5, y=-1, demand = 1))
  geo<-add(geo, new("Customer", id="K08", x=-1, y=-3, demand = 1))
  geo<-add(geo, new("Customer", id="K09", x=3, y=-2, demand = 1))
  geo<-add(geo, new("Customer", id="K10", x=6, y=-1, demand = 1))
  
  cap=4
  dur=16
  
  geo<-VRP.SWEEP(geo, digits=1, log=FALSE, constraint.dur= dur, constraint.cap = cap)
  vrp<-geo$warehouses$vrp[[1]]
  
  #different results as in literature! 
  #Domschke executes a two opt after temorarilay adding the new node.
  #this is implemented (yet)
  
  expect_true(vrp$F == 55.7)
  expect_true(vrp$cij[1,2] == 4.1)
  expect_true(length(vrp$tours) == 4)
  expect_true(vrp$tours[[1]]$loading == 3) 
  expect_equal(vrp$tours[[1]]$stops.indices,c(2,3,4))
  
}  
)


context("\tTest 02: Does Savings-Algorithm work properly?") 
test_that("Savings works correctly", {  
  
  geo<-new("GeoSituation")
  #example taken from Domschke, Wolfgang (2010): Logistik. Rundreisen und Touren. 5., Aufl. Muenchen [u.a.]: Oldenbourg (2). S. 236f
  geo<-add(geo, new("Warehouse", id="L", x=0, y=0, supply=10))
  geo<-add(geo, new("Customer", id="K01", x=4, y=1, demand = 1))
  geo<-add(geo, new("Customer", id="K02", x=1, y=2, demand = 1))
  geo<-add(geo, new("Customer", id="K03", x=0, y=5, demand = 1))
  geo<-add(geo, new("Customer", id="K04", x=-3, y=3, demand = 1))
  geo<-add(geo, new("Customer", id="K05", x=-2, y=1, demand = 1))
  geo<-add(geo, new("Customer", id="K06", x=-5, y=1, demand = 1))
  geo<-add(geo, new("Customer", id="K07", x=-5, y=-1, demand = 1))
  geo<-add(geo, new("Customer", id="K08", x=-1, y=-3, demand = 1))
  geo<-add(geo, new("Customer", id="K09", x=3, y=-2, demand = 1))
  geo<-add(geo, new("Customer", id="K10", x=6, y=-1, demand = 1))
  
  cap=4
  dur=16
  deltaX <- 4
  deltaY <- 3
  alpha <- 1
  
  geo<-VRP.SAVINGS(geo, digits=2, log=FALSE, constraint.dur= dur, constraint.cap = cap, deltaX = deltaX, deltaY = deltaY, alpha = alpha)
  
  vrp<-geo$warehouses$vrp[[1]]
  #different results as in literature due to rounding errors
  
  expect_true(round(vrp$F) == round(44.85))
  expect_true(vrp$cij[1,2] == 4.12)
  expect_true(length(vrp$tours) == 16)
  expect_true(vrp$tours[[16]]$loading == 2) 
  expect_equal(vrp$tours[[16]]$stops.indices,c(4,3))
  
}  
)
