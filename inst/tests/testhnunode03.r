context("Test 03:\tNodes Methods")

#setup nodes
#
context("\t\tcreate test objects") 
n1 <- new("HNUNode",x=10,y=20, id ="n1")
n2 <- new("HNUNode",x=13,y=24, id ="n1")

 
context("\t\tTest Distance-Calculation") 
test_that("Distances are correct", {
  	expect_that(calc.Distance(n1,n2), equals(5))  
})
  
context("\t\tTest Cost-Calculation")  
test_that("Costs are correct", {
  expect_that(calc.Distance(n1,n2, costs = 2), equals(10))  
})  

context("Test 03: Passed.")
