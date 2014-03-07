#create some nodes
n1 <- new("Node",x=10,y=20, id ="n1")
n2 <- new("Node",x=13,y=24, id ="n1")

# calculate Beeline distance

getDistance(n1,n2) # should result 5
getDistance(n1,n2, costs = 2) # should result 10
