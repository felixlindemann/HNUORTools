options(stringsAsFactors=FALSE) #WICHTIG!!
context("Testing  Warehouse Location Problem ")  
 

context("\tTest 01: Does ADD-Algorithm work properly?") 
test_that("Add works correctly", {  
  set.seed(1)
  geo<-new("GeoSituation")
  #example taken from Domschke, Wolfgang (1996): Logistik. Standorte. 4. Aufl. Muenchen: Oldenbourg (3)  S.63
  for(i in 1:7){
    geo<- add(geo, new("Customer", id=i ))
  }
  f<-c(5,7,5,6,5)
  for(i in 1:5){
    geo<- add(geo, new("Warehouse", id=i , fixcosts=f[i]))
  }
  cij<- matrix(
          c(
            1,2,10,9,6,7,3,
            2,9,0,7,3,6,10,
            7,6,1,5,3,10,5,
            6,5,10,2,6,3,6,
            6,4,6,3,7,2,6
            ), 
          ncol=7, byrow=TRUE)
  
  geo<-WLP.ADD(geo, cij =cij)
  
  
  expect_true(geo$wlp$x[1,1] == 1)
  expect_true(geo$wlp$x[1,2] == 1)
  expect_true(geo$wlp$x[2,3] == 1)
  expect_true(geo$wlp$x[5,4] == 1)
  expect_true(geo$wlp$x[2,5] == 1)
  expect_true(geo$wlp$x[5,6] == 1)
  expect_true(geo$wlp$x[1,7] == 1)
  
  expect_true(geo$wlp$F == 31)
  expect_true(length(which(geo$wlp$y == c(1,1,0,0,1)))==5)
  expect_true(length(which(geo$wlp$I1 == c(5,2,1)))==3)
  expect_true(length(which(geo$wlp$I0 == c(4,3)))==2)
  
}	
)