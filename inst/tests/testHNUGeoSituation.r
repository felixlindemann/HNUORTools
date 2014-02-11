context("Testing HNU GeoSituation")  

#create 4 Nodes

# HNU GeoSituation

geo<-HNUGeoSituation.create()

context("\tTest 01: can objects be added correctly?") 
test_that("Test for adding objects to geoSituation", {
	geo<-add(geo,new("HNUNode"))
  	expect_true(length(geo$nodes) == 1)
	geo<-add(geo,new("HNUNode"))
  	expect_true(length(geo$nodes) == 2)
	geo<-add(geo,new("HNUWarehouse"))
  	expect_true(length(geo$nodes) == 2)
  	expect_true(length(geo$warehouses) == 1)
	geo<-add(geo,new("HNUWarehouse"))
  	expect_true(length(geo$nodes) == 2)
  	expect_true(length(geo$warehouses) == 2)
	geo<-add(geo,new("HNUCustomer"))
  	expect_true(length(geo$nodes) == 2)
  	expect_true(length(geo$warehouses) == 2)
  	expect_true(length(geo$customers) == 1)
	geo<-add(geo,new("HNUCustomer"))
  	expect_true(length(geo$nodes) == 2)
  	expect_true(length(geo$warehouses) == 2)
  	expect_true(length(geo$customers) == 2)

}) 

context("done.")   
context("--------------------------------------------------")  



