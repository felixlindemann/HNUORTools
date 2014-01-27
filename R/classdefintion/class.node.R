HNU.Knoten <- setRefClass("HNU.Knoten",
  fields = list( 
    id    = "character", 
    x     = "numeric",
    y     = "numeric",
    label = "character"
  ),
  methods = list(
    isValid = function(){
      return(TRUE)
    },
    calcDistance = function(andererKnoten, Kostensatz = 1) {
      'Berechnet die Luftlinienentfernung zwischen zwei Knoten. Wenn Kostensatz <> 1 kann die Luftlinienentfernung auch als Kosten für diee Fahrt von A nach B interpretiert werden.'
      if(missing(andererKnoten)){
        simpleError(paste("Fehler. Das Argument 'andererKnoten' fehlt. Die Berechnung ist daher nicht möglich."))
      }
      
      dist <- sqrt((x - andererKnoten$x)^2+(y - andererKnoten$y )^2) 

      #'Method for automatically printing matrix editors'
      #cat("Reference matrix editor object of class",
      #classLabel(class(.self)), "\n")
      #cat("Data: \n")
      #methods::show(data)
      #cat("Undo list is of length", length(edits), "\n")
      #
      return (dist * Kostensatz)
    }
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

k1$calcDistance(k2)
