HNU.Lager <- setRefClass("HNU.Lager",
  fields = list( 
    Angebot   = "numeric",
    FixKosten = "numeric",
    isDummy   = "logical"
  ),
  contains = "HNU.Knoten",
  methods = list(
    isValid = function(){
      return(TRUE)
    }
  )
)


l1 <- HNU.Lager$new(
  id = "l1",
  x = 10,
  y = 10,
  label = "Lager 1",
  Angebot = 10 
)
l2 <- HNU.Lager$new(
  id = "l2",
  x = 12,
  y = 13,
  label = "Lager 2",
  Angebot = 20
)

l1$calcDistance(l2)
