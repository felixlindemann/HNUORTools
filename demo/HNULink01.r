 
#create 4 Nodes
 

n1<- HNUNode.create(x=10, y=20)
n2<- HNUNode.create(x=13, y=24)
n3<- new("HNUNode", x=7, y=16)
n4<- new("HNUNode", x=7, y=20)
 
	l1 <- HNULink.create(n1,n2)
	l2 <- HNULink.create(n1,n3)
	l3 <- HNULink.create(n1,n4)
	l4 <- HNULink.create(n3,n4)
  
	l1$distance == 5
	l2$distance == 5
	l3$distance == 3
	l4$distance == 4
 