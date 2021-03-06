options(stringsAsFactors=FALSE) #WICHTIG!!
context("Testing  Travelling Salesman Problem ")  

#create 4 Nodes

#  GeoSituation

context("\tTest 01: is solution for a TSP calculated with TSP-Nearest-Neighbor Method correctly?") 
test_that("Test for TSP-Nearest-Neighbor Method", {  
  
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
  
  geo$tsp<- TSP.NearestNeighbor(geo, nodes =geo$nodes, StartNode = s.node)
  
  expect_true(round(geo$tsp$F) == round(631.8579))
  
  expect_true(geo$tsp$x[1,6] == 1)
  expect_true(geo$tsp$x[2,3] == 1)
  expect_true(geo$tsp$x[4,2] == 1)
  
}	
)
context("\tTest 02 TSP-Nearest-Neighbor Method + 2opt") 
test_that("Test for TSP-Nearest-Neighbor Method + 2opt", {  
  
  data(bordersgermany)
  geo<-new("GeoSituation")
  #example taken from
  
  
  #chemnitz, cottbus, dresden, erfurt, gera, halle leipzig
  A<- c(10, 11, 14, 17, 24, 22, 35)
  
  for(a in A){
    df <- cities[a, ]
    geo<-add(geo,new("Node", id=df$id, label = df$label, x=df$x,   y=df$y))
  }
  t.costs <- 1
  s.node <- 3 # index of dresden in geo$nodes
  
  geo$tsp<- TSP.NearestNeighbor(geo, nodes =geo$nodes, StartNode = s.node)
  
  expect_true(round(geo$tsp$F) == round(631.8579))
  
  expect_true(geo$tsp$x[1,6] == 1)
  expect_true(geo$tsp$x[2,3] == 1)
  expect_true(geo$tsp$x[4,2] == 1)
  
  geo$tsp<-TSP.2OPT(geo, tsp=geo$tsp)
  expect_true(round(geo$tsp$F) == round(540.7254))
  expect_true(geo$tsp$x[1,6] == 1)
  expect_true(geo$tsp$x[2,3] == 1)
  expect_true(geo$tsp$x[4,2] == 0)
  
  
}	
)

context("\tTest 02 TSP-Nearest-Neighbor Method + 3opt ?") 
test_that("Test for TSP-Nearest-Neighbor Method + 3opt", {  
  
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
  
  geo$tsp<- TSP.NearestNeighbor(geo, nodes =geo$nodes, StartNode = s.node)
  
  expect_true(round(geo$tsp$F) == round(631.8579))
  
  expect_true(geo$tsp$x[1,6] == 1)
  expect_true(geo$tsp$x[2,3] == 1)
  expect_true(geo$tsp$x[4,2] == 1)
  
  geo$tsp<-TSP.3OPT(geo, tsp=geo$tsp)
  expect_true(round(geo$tsp$F) == round(540.7254))
  expect_true(geo$tsp$x[1,6] == 1)
  expect_true(geo$tsp$x[2,3] == 1)
  expect_true(geo$tsp$x[4,2] == 0)
  
  
}	
)