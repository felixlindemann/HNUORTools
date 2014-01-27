HNU.Kunde <- setRefClass("HNU.Kunde",
  fields = list( 
    Nachfrage = "numeric",
    isDummy = "logical"
  ),
  contains = "HNU.Knoten",
  methods = list(
    isValid = function(){
      return(TRUE)
    }
  )
)


k1 <- HNU.Kunde$new(
  id = "k1",
  x = 10,
  y = 10,
  label = "Kunde 1",
  Nachfrage = 10 
)
k2 <- HNU.Kunde$new(
  id = "k2",
  x = 12,
  y = 13,
  label = "Kunde 2",
  Nachfrage = 20
)

k1$calcDistance(k2)
