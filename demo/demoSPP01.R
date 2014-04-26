geo<-new("GeoSituation")
# Create some Nodes

geo<-add(geo,new("Node",id = "n1", x=10, y=20))
geo<-add(geo,new("Node",id = "n2",x=13, y=24))
geo<-add(geo,new("Node", id = "n3",x=7, y=16))
geo<-add(geo,new("Node", id = "n4",x=7, y=20))

# Add Some Links
geo <- add(geo, new("Link",geo$nodes[1],geo$nodes[2]))
geo <- add(geo, new("Link",geo$nodes[1],geo$nodes[3]))
geo <- add(geo, new("Link",geo$nodes[1],geo$nodes[4]))
geo <- add(geo, new("Link",geo$nodes[3],geo$nodes[4]))



#solve Dijkstra 
startindex <- sample(1:4,1) # randomize the task
geo <- SPP.Dijkstra(geo,start=startindex, log=FALSE)

#Plot the geosituation
plotGeoSituation(
  geo, 
  main="Demo shortest path problem", 
  sub = paste("iteration:", geo$spp$iter),
  drawLinks = TRUE,
  drawNodes = TRUE,
  lines.plotlength = TRUE
) 
#mark StartNode
points(geo$nodes[startindex]$x,geo$nodes[startindex]$y,pch = 21, bg = 2 , cex=4)

#redraw nodes
drawNodes(geo)
