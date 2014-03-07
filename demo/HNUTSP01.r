	geo<-new("GeoSituation")
 	#example taken from
 	
 	data(bordersgermany)

 	#chemnitz, cottbus, dresden, erfurt, gera, halle leipzig
 	A<- c(10, 11, 14, 17, 24, 22, 35)

 	for(a in A){
 		df <- cities[a, ]
 		geo<-add(geo,new("Node", id=df$id, label = df$label, x=df$x,   y=df$y))
 	}
 	t.costs <- 1
 	s.node <- 3 # index of dresden in geo$nodes

 	geo<- TSP.NearestNeighbor(geo, nodes=geo$nodes, StartNode = s.node)


	plotGeoSitatuon.bordersgermany( 
		pch=20, cex=1,
		xlim=range(cities[A, "x"]), 
		ylim=range(cities[A, "y"])+c(0,10)
	)

	TSP.drawrouting(geo)
	text(cities[A,"x"],cities[A,"y"]+5, cities[A,"id"],col=2)

	#2opt
	geo<-TSP.2OPT(geo)
	TSP.drawrouting(geo, col=2)
 