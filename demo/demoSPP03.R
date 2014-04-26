options(stringsAsFactors=FALSE) #WICHTIG!!
# SoSe 2014 Tutorium 1 TPP
# Aufg. 3)
# 
#################################################################################### 
#  
set.seed(5)  		# Zufallszahl setzen, #8, #10 gehen
bounds<-c(0,150)	# Grenzen des Szenarios
N<-8				# Anzahl Knoten 

x <- sample(bounds[1]:bounds[2],N)
y <- sample(bounds[1]:bounds[2],N)
 
####################################################################################
geo<-new("GeoSituation")

geo<-add(geo, new("Node", id = paste("N", 1:N, sep=""), x=x, y=y))
adj<- matrix(rep(0,(N)^2), ncol=(N)) 
rownames(adj) <-  geo$nodes$id
colnames(adj) <-  geo$nodes$id

#################################################################################### 

adj[1,3]<-1
adj[1,4]<-1 
adj[1,6]<-1
adj[1,7]<-1

adj[2,7]<-1
adj[2,8]<-1

adj[3,8]<-1

adj[4,5]<-1

adj[5,7]<-1

adj[6,7]<-1
adj[6,8]<-1
 
#####################################################################################

#setzen der Verbindungen
for(i in 1:nrow(adj)){
  for(j in 1:nrow(adj)){
    if(adj[i,j] == 1){
      n1<-geo$nodes[i]
      n2<-geo$nodes[j]
      l1 <- new("Link", n1,n2, id = paste("L",(length(geo$links)+1) ,sep=""), digits=0)
      
      l1$distance <- round(l1$distance) #runden auf ganze Zahlen
      l1$costs <- round(l1$costs) #runden auf ganze Zahlen
      
      geo<-add(geo,l1)
    }
  }
}


plotGeoSituation(geo, lines.plotlength=TRUE, main="Geografische Situation des Szenarios.", drawNodes = TRUE)

startindex <- 5  
geo <- SPP.Dijkstra(geo,start=startindex, log=TRUE,debug=TRUE, plot=FALSE, main="SoSe 2014 Tutorium 1 TPP\nAufg. 3")

# dev.new()

plotGeoSituation(geo,
     main="SoSe 2014 Tutorium 1 TPP\nAufg. 3", 
     sub="Solution",
     drawNodes = TRUE,
     drawLinks = TRUE, 
     lines.plotlength=TRUE)

#mark StartNode
points(geo$nodes[startindex]$x,geo$nodes[startindex]$y,pch = 21, bg = 2 , cex=4)

#redraw nodes
drawNodes(geo)


