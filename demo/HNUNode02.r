df<- data.frame(x=10,y=20)

new("HNUNode", df)

as(df, "HNUNode")

as.HNUNode(df)

HNUNode.create(x=20, y=30)