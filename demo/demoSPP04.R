options(stringsAsFactors=FALSE) #WICHTIG!!
# SoSe 2014 Tutorium 1 TPP
# Aufg. 4)   
N<-7				# Anzahl Knoten    
geo<-new("GeoSituation") 
#create Nodes
geo<-add(geo, new("Node", id = LETTERS[1:N], x= c(0,10,10,20,30,30,40), y=c(0,10,-10,0,10,-10,0)))
#add links
geo<-add(geo,new("Link", geo$nodes[1],geo$nodes[2], distance=6, costs= 6))
geo<-add(geo,new("Link", geo$nodes[1],geo$nodes[3], distance=2, costs= 2, used=TRUE))
geo<-add(geo,new("Link", geo$nodes[2],geo$nodes[3], distance=5, costs= 5))
geo<-add(geo,new("Link", geo$nodes[2],geo$nodes[4], distance=3, costs= 3, used=TRUE))
geo<-add(geo,new("Link", geo$nodes[2],geo$nodes[5], distance=1, costs= 1, used=TRUE))
geo<-add(geo,new("Link", geo$nodes[3],geo$nodes[4], distance=4, costs= 4, used=TRUE))
geo<-add(geo,new("Link", geo$nodes[3],geo$nodes[6], distance=5, costs= 5))
geo<-add(geo,new("Link", geo$nodes[4],geo$nodes[5], distance=4, costs= 4))
geo<-add(geo,new("Link", geo$nodes[4],geo$nodes[6], distance=5, costs= 5))
geo<-add(geo,new("Link", geo$nodes[5],geo$nodes[6], distance=6, costs= 6))
geo<-add(geo,new("Link", geo$nodes[5],geo$nodes[7], distance=1, costs= 1, used=TRUE))
geo<-add(geo,new("Link", geo$nodes[6],geo$nodes[7], distance=1, costs= 1, used=TRUE))

#plot(default Szeanrio)
plot(geo, 
     lines.plotlength=TRUE, 
     main="Geografische Situation des Szenarios.", 
     lines.markused = TRUE, 
     colused="blue", col=1)

for(i in 1:length(geo$links)){
  geo$links[[i]]$used<-FALSE
}

# solve Dijsktra
startindex <- 1  
geo <- SPP.Dijkstra(geo,start=startindex, log=TRUE,debug=TRUE, plot=FALSE, main="SoSe 2014 Tutorium 1 TPP\nAufg. 3")

# plot Solution

plot(geo,
     main="SoSe 2014 Tutorium 1 TPP\nAufg. 3", 
     sub="Solution",
     drawNodes = TRUE,
     drawLinks = TRUE, 
     lines.plotlength=TRUE)

#mark StartNode
points(geo$nodes[startindex]$x,geo$nodes[startindex]$y,pch = 21, bg = 2 , cex=4) 
#redraw nodes
drawNodes(geo)
 