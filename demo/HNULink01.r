 
#create 4 Nodes
 

n1<- new("Node",x=10, y=20)
n2<- new("Node",x=13, y=24)
n3<- new("Node", x=7, y=16)
n4<- new("Node", x=7, y=20)
 
	l1 <- new("Link",n1,n2)
	l2 <- new("Link",n1,n3)
	l3 <- new("Link",n1,n4)
	l4 <- new("Link",n3,n4)
  
	l1$distance == 5
	l2$distance == 5
	l3$distance == 3
	l4$distance == 4
 