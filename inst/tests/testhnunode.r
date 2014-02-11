context("Testing HNUNodes.")  

# create a new Node with specific values"
node<- new("HNUNode", x= 10, y=20, id="myid", label = "mylabel")

# test_that('Coordinates are correct')"
context("\tTest 01: Are Objects correctly created?")
test_that("Coordinates are correct", {

	expect_equal(node$x, 10)
	expect_equal(node$y, 20)
	expect_false(node$x == 12)
	expect_false(node$y == 12)

})
    
#setup dataframe.
#
# create test objects"
df<- data.frame(x=10,y=20, id ="myid", label ="mylabel")
li<- list(x=10,y=20, id ="myid", label ="mylabel")

node01 <- new("HNUNode", df)
node02 <-  as(df, "HNUNode") 
node03 <- as.HNUNode(df)


context("\tTest 02: Are Objects correctly converted from a data.frame?")
test_that("Coordinates are correct", {
  expect_that(node01$x, equals(df$x)) 
  expect_that(node01$y, equals(df$y)) 
})
 
test_that("Conversion are correct", {
  expect_identical(node01, node02)
  expect_identical(node01, node03)
  expect_identical(node02, node03)
})

context("\tTest 03: Are Objects correctly converted from a list?")

node01 <- new("HNUNode", li)
node02 <- as(li, "HNUNode") 
node03 <- as.HNUNode(li)
 
test_that("Is.HNUNode working", {
  expect_false(is(li, "HNUNode"))
  expect_true(is(node01, "HNUNode"))
  expect_true(is.HNUNode(node01))
})


context("\tTest 04: Are Objects correctly converted back and forwards?")
node.orignal<- HNUNode.create() #random node
df<- as.data.frame(node.orignal) 
 
test_that("Back-Forward Conversion is working", {
    expect_that(node.orignal, equals(as.HNUNode(df)))  
    expect_that(node.orignal, equals(new("HNUNode",df)))
    expect_that(node.orignal, equals(as(df,"HNUNode")))
})

context("\tTest 05: Are Node-Methods working correctly?") 
n1 <- new("HNUNode",x=10,y=20, id ="n1")
n2 <- new("HNUNode",x=13,y=24, id ="n1")
 
test_that("Distances are calculated correctly", {
  	expect_that(calc.Distance(n1,n2), equals(5))  
})
   
test_that("Costs are calculated correctly", {
  expect_that(calc.Distance(n1,n2, costs = 2), equals(10))  
})   

context("done.") 
context("--------------------------------------------------")  