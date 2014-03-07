context("Testing Customer")  

#create a random Customer with user friendly function 
#	-- showwarnings=TRUE to indicate this. Default is FALSE
#	
# context("\t\tcreate a random Customer with user friendly function ") 
c1<- new("Customer",showwarnings=FALSE)


# context("\t\tcreate an explizit defined Customer with S4 initializer")  
c2<- new("Customer",id = "C02", label="Customer 02", x = 10, y= 20, demand = 10 )


# context("\t\tcreate a Customer from a data.frame")  
df <- data.frame(id = "C02", label="Customer 02", x = 14, y= 23, demand = 10 )
c3 <- new("Customer",df)


# context("\t\tconvert Customer to data.frame")   
df <- as.data.frame(c3)

# context("\t\tor convert a data.frame to a Customer")   
c3a <- as.Customer(df)


#Do some testing
#
context("\tTest 01: Are Objects created correctly?") 
test_that("Test for identical objects", {
  expect_false(identical(c1,c3)) # Customer 1 and 3  should NOT be idenitical
  expect_true(identical(c3,c3a)) # Customer 3 and 3a should be idenitical
  
})

context("\tTest 02: Are the Node-Methods for Customer working?") 
test_that("Node-Methods work for Customer", {
   
  expect_true(getDistance(c2,c3) == 5)  			   # should be 5
	# use a cost-factor: 
  expect_true(getDistance(c2,c3, costfactor = 2) == 10)   # should be 10 
})
context("\tTest 03: Are the Customers correctly validated?") 
test_that("Validation is implemented correctly", {
   
  	expect_error(c1$demand <- -12, "invalid class “Customer” object: Error with value demand: expected numeric non negative value, but obtained -12")
})

context("done.")   
context("--------------------------------------------------")  

