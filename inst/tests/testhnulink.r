context("Testing HNU Links")  

#create 4 Nodes

# HNU Link

n1<- HNUNode.create(x=10, y=20)
n2<- HNUNode.create(x=13, y=24)
n3<- new("HNUNode", x=7, y=16)
n4<- new("HNUNode", x=7, y=20)

#create links
#
	l1 <- HNULink.create(n1,n2)
	l2 <- HNULink.create(n1,n3)
	l3 <- HNULink.create(n1,n4)
	l4 <- HNULink.create(n3,n4)
context("\tTest 01: Are the Node-Methods for Links working?") 
test_that("Node-Methods work for Links", {

	expect_true(l1$distance == 5)
	expect_true(l2$distance == 5)
	expect_true(l3$distance == 3)
	expect_true(l4$distance == 4)
 
}) 

context("done.")   
context("--------------------------------------------------")  


