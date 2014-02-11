#create some nodes
n1 <- new("HNUNode",x=10,y=20, id ="n1")
n2 <- new("HNUNode",x=13,y=24, id ="n1")

# calculate Beeline distance

calc.Distance(n1,n2) # should result 5
calc.Distance(n1,n2, costs = 2) # should result 10
