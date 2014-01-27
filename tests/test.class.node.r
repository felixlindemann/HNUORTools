
# should work fine
obj1 <- new("HNUNode", x=12, y=10, label="myNode", id="N0001")
obj1


#should throw an error -> Arrays not supported here
obj2 <- new("HNUNode", x=c(12,10), y=c(10,12), label="myNode", id="N0002")
obj2