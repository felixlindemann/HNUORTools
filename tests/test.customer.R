# should work fine
k1 <- new("HNUCustomer", x=12, y=10, label="myNode", id="N0001", demand = 30)
k1
# should raise error
k2 <- new("HNUCustomer", x=c(12,12), y=c(10,10), label="myNode.error", id="N000.Error", demand = 30)
k3 <- new("HNUCustomer", x=12, y=10, label="myNode.error", id="N0001.error", demand = -30)

# should work fine
obj1 <- new("HNUNode", x=12, y=10, label="myNode", id="N0001")
obj1

# should work fine
k4 <- new("HNUCustomer", location=obj1, demand = 30)
k4

