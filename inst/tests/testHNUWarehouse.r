context("Testing Warehouse")  

#create a random Warehouse with user friendly function 
#	-- showwarnings=TRUE to indicate this. Default is FALSE
#	
# context("\t\tcreate a random Warehouse with user friendly function ") 
w1<- new("Warehouse",showwarnings=FALSE)


# context("\t\tcreate an explizit defined Warehouse with S4 initializer")  
w2<- new("Warehouse",id = "W02", label="Warehouse 02", x = 10, y= 20, supply = 10, fixcosts = 1000 )


# context("\t\tcreate a Warehouse from a data.frame")  
df <- data.frame(id = "W02", label="Warehouse 02", x = 14, y= 23, supply = 10, fixcosts = 1000 )
w3 <- new("Warehouse",df)


# context("\t\tconvert Warehouse to data.frame")   
df <- as.data.frame(w3)

# context("\t\tor convert a data.frame to a Warehouse")   
w3a <- as.Warehouse(df)


#Do some testing
#
context("\tTest 01: Are Objects created correctly?") 
test_that("Test for identical objects", {
  expect_false(identical(w1,w3)) # Warehouse 1 and 3  should NOT be idenitical
  expect_true(identical(w3,w3a)) # Warehouse 3 and 3a should be idenitical
  
})

context("\tTest 02: Are the Node-Methods for Warehouse working?") 
test_that("Node-Methods work for Warehouse", {
   
  expect_true(calc.Distance(w2,w3) == 5)  			   # should be 5
	# use a cost-factor: 
  expect_true(calc.Distance(w2,w3, costfactor = 2) == 10)   # should be 10 
})
context("\tTest 03: Are the Warehouses correctly validated?") 
test_that("Validation is implemented correctly", {
   
  	expect_error(w1$supply   <- -12, "invalid class “Warehouse” object: Error with value supply: expected numeric non negative value, but obtained -12")
 	expect_error(w1$fixcosts <- -12, "invalid class “Warehouse” object: Error with value fixcosts: expected numeric non negative value, but obtained -12")
})

context("done.")   
context("--------------------------------------------------")  

