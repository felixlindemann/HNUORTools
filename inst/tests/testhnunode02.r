context("Test 02:\tConverting Nodes")

#setup dataframe.
#
context("\t\tcreate test objects") 
df<- data.frame(x=10,y=20, id ="myid", label ="mylabel")


node01 <- new("HNUNode", df)
node02 <- as(df, "HNUNode") 
node03 <- as.HNUNode(df)

context("\t\tCoordinates are correct") 
test_that("Coordinates are correct", {
  expect_that(node01$x, equals(df$x)) 
  expect_that(node01$y, equals(df$y)) 
})
context("\t\tConversion are correct") 
test_that("Conversion are correct", {
  expect_identical(node01, node02)
  expect_identical(node01, node03)
  expect_identical(node02, node03)
})

context("\t\tIs.HNUNode working") 
test_that("Is.HNUNode working", {
  expect_false(is(df, "HNUNode"))
  expect_true(is(node01, "HNUNode"))
  expect_true(is.HNUNode(node01))
})

context("Test 02: Passed.")
