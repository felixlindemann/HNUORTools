HNU.Kante <- setRefClass("HNU.Kante",
  fields = list( 
    id    = "character", 
    from  = "HNU.Knoten",
    to    = "HNU.Knoten",
    distance = "numeric",
    oneWay = "logical",
    cost = "numeric"
  ),
  methods = list(
    isValid = function(){
      return(TRUE)
    },
    setDistance = function(Kostensatz = 1){
      distance <- from$calcDistance(to)
      cost <- distance * Kostensatz
    } 
  )
)

Kostensatz <- 1

Knoten <- list()
Knoten[1] <- HNU.Knoten$new(
  id = "k1",
  x = 10,
  y = 10,
  label = "Kunde 1"  
)
 
Knoten <- list()
Knoten <- rbind(Knoten, HNU.Knoten$new(
  id = "k1",
  x = 10,
  y = 10,
  label = "Kunde 1"  
)
)

k1 <- HNU.Knoten$new(
  id = "k1",
  x = 10,
  y = 10,
  label = "Kunde 1"
)
k2 <- HNU.Knoten$new(
  id = "k2",
  x = 12,
  y = 13,
  label = "Kunde 2"
) 
k3 <- HNU.Knoten$new(
  id = "k3",
  x = 8,
  y = 6,
  label = "Kunde 3"
)  
k4 <- HNU.Knoten$new(
  id = "k4",
  x = 7,
  y = 10,
  label = "Kunde 4"
)
k5 <- HNU.Knoten$new(
  id = "k5",
  x = 12,
  y = 4,
  label = "Kunde 5"
) 
k6 <- HNU.Knoten$new(
  id = "k6",
  x = 0,
  y = 0,
  label = "Kunde 6"
) 

Knoten <- as.list(c(k1,k2,k3,k4,k5,k6))

l1 <- HNU.Kante$new(
  from = k1,
  to   = k2,
  oneWay = FALSE
)

l2 <- HNU.Kante$new(
  from = k1,
  to   = k3,
  oneWay = FALSE
)
l3 <- HNU.Kante$new(
  from = k2,
  to   = k3,
  oneWay = FALSE
)
l4 <- HNU.Kante$new(
  from = k2,
  to   = k4,
  oneWay = FALSE
)
l5 <- HNU.Kante$new(
  from = k3,
  to   = k4,
  oneWay = FALSE
)
l6 <- HNU.Kante$new(
  from = k4,
  to   = k5,
  oneWay = FALSE
)

l1$setDistance(Kostensatz)
l2$setDistance(Kostensatz)
l3$setDistance(Kostensatz)
l4$setDistance(Kostensatz)
l5$setDistance(Kostensatz)
l6$setDistance(Kostensatz)




