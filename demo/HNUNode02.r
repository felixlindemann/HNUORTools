df<- data.frame(x=10,y=20)

new("Node", df)

as(df, "Node")

as.Node(df)

new("Node", x=20, y=30)