options(stringsAsFactors=FALSE) #WICHTIG!!
context("Testing  SPP Dijkstra")  
 
# setup

# geo<-add(geo,l5) # produces warnung as expected. --> Works.


context("\tTest 01: Does SPP.Dijkstra work?") 
test_that("Distances are calculated as expected.", {
  
  set.seed(10)  		# Zufallszahl setzen, #8, #10 gehen
  bounds<-c(0,150)	# Grenzen des Szenarios
  N<-8				# Anzahl Knoten 
  
  
  geo<-new("GeoSituation") 
  for( i in 1:N){ 
    n1<- new("Node", id = paste("N",i, sep=""), x=sample(bounds[1]:bounds[2],1), y=sample(bounds[1]:bounds[2],1))
    geo<-add(geo,n1) 
  } 
  adj<- matrix(rep(0,(N)^2), ncol=(N)) 
  rownames(adj) <-  geo$nodes$id
  colnames(adj) <-  geo$nodes$id
  
  adj[1,3]<-1
  adj[1,4]<-1 
  adj[1,6]<-1
  adj[1,7]<-1
  
  adj[2,7]<-1
  
  adj[3,8]<-1
  
  adj[4,5]<-1
  
  adj[5,7]<-1
  
  adj[6,2]<-1
  adj[6,7]<-1
  adj[6,8]<-1
  
  
  #setzen der Verbindungen
  for(i in 1:nrow(adj)){
    for(j in 1:nrow(adj)){
      if(adj[i,j] == 1){
        n1<-geo$nodes[i]
        n2<-geo$nodes[j]
        l1 <- new("Link",n1,n2, id = paste("L",(length(geo$links)+1) ,sep=""))
        
        l1$distance <- round(l1$distance) #runden auf ganze Zahlen
        l1$costs <- round(l1$costs) #runden auf ganze Zahlen
        
        geo<-add(geo,l1)
      }
    }
  }
  geo <- SPP.Dijkstra(geo,start=5, log=FALSE)
  
  expect_equal(geo$spp$tableau[1,1], 58)
  expect_equal(geo$spp$tableau[1,2], 7)
  
  expect_equal(geo$spp$tableau[2,1], 85)
  expect_equal(geo$spp$tableau[2,2], 7)
  
  expect_equal(geo$spp$tableau[3,1], 143)
  expect_equal(geo$spp$tableau[3,2], 1)
  
  expect_equal(geo$spp$tableau[4,1], 81)
  expect_equal(geo$spp$tableau[4,2], 5)
  
  expect_equal(geo$spp$tableau[5,1], 0)
  expect_equal(geo$spp$tableau[5,2], 0)
  
  expect_equal(geo$spp$tableau[6,1], 104)
  expect_equal(geo$spp$tableau[6,2], 7)
  
  expect_equal(geo$spp$tableau[7,1], 51)
  expect_equal(geo$spp$tableau[7,2], 5)
  
  expect_equal(geo$spp$tableau[8,1], 166)
  expect_equal(geo$spp$tableau[8,2], 6)
   
}) 



context("\tTest 02: Does SPP.TRIPLE work?") 
test_that("Distances are calculated as expected.", {
   # Taken from Domschke, Wolfgang; Drexl, Andreas (2005): Einfuehrung in Operations Research. Mit 63 Tabellen. 6., ueberarb. und erw. Aufl. Berlin: Springer.
  # p.76ff
  geo<-new("GeoSituation") 
  
  geo <- add(geo, new("Node", id="N1", x= 0, y= 0))
  geo <- add(geo, new("Node", id="N2", x= 10, y= +5))
  geo <- add(geo, new("Node", id="N3", x= 20, y= +5))
  geo <- add(geo, new("Node", id="N4", x= 10, y= -5))
  geo <- add(geo, new("Node", id="N5", x= 20, y= -5))

  geo <- add(geo, new("Link", geo$nodes[1], geo$nodes[2], distance = 20, oneway=TRUE))
  geo <- add(geo, new("Link", geo$nodes[1], geo$nodes[4], distance = 10, oneway=TRUE))
  geo <- add(geo, new("Link", geo$nodes[2], geo$nodes[3], distance = 20, oneway=TRUE))
  geo <- add(geo, new("Link", geo$nodes[2], geo$nodes[5], distance = 50, oneway=TRUE))
  geo <- add(geo, new("Link", geo$nodes[3], geo$nodes[5], distance = 10, oneway=TRUE))
  geo <- add(geo, new("Link", geo$nodes[4], geo$nodes[2], distance = 20, oneway=TRUE))
  geo <- add(geo, new("Link", geo$nodes[4], geo$nodes[5], distance = 50, oneway=TRUE))
  geo <- add(geo, new("Link", geo$nodes[5], geo$nodes[3], distance = 20, oneway=TRUE))

  geo<- SPP.TRIPLE(geo)
  

}) 



context("done.")   
context("--------------------------------------------------")  
 