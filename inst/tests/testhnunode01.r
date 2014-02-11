context("Test 01:\tCreating Nodes")

context("\t\tcreate a new Node with specific values") 
node<- new("HNUNode", x= 10, y=20, id="myid", label = "mylabel")

context("\t\ttest_that('Coordinates are correct')") 
test_that("Coordinates are correct", {

	expect_equal(node$x, 10)
	expect_equal(node$y, 20)
	expect_false(node$x == 12)
	expect_false(node$y == 12)

})
   
context("Test 01: Passed.")
