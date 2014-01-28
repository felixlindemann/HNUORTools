
print("Starte Klassentest HNU NODE")
tryCatch(
	{
		print("Test 1:  Erzeuge ein Objekt vom Typ HNU NODE.")
		print(" 		Dieser Test sollte keine Fehler prodzuieren.")
		obj1 <- new("HNUNode", x=12, y=10, label="myNode", id="N0001")	
	},	
	warning = function(w) {
		print(paste("unerwartete Warnung:\n", w))
	}, 
	error 	= function(e) {
		print(paste("unerwarteter Fehler:\n", e))
		stop(e)
	}, 
	finally	= {
		print("Test 1:  beendet.")
	}
)

tryCatch(
	{
		print("Test 2:  Erzeuge ein Objekt vom Typ HNU NODE.")
		print(" 		Dieser Test sollte einen Fehler prodzuieren, da die Initialisierung des Objektes falsch ist.")
		obj2 <- new("HNUNode", x=c(12,10), y=c(10,12), label="myNode", id="N0002")
		simpleWarning  ("Dies Stelle des Quellcode hätte nicht erreicht werden dürfen.")
	},	
	warning = function(w) {
		print(paste("unerwartete Warnung:\n", w))
		stop(w)
	}, 
	error 	= function(e) {
		print(paste("erwartete Fehlermeldung:\n", e))
		
	}, 
	finally	= {
		print("Test 2:  beendet.")
	} 
)
print("Klassentest HNU NODE - beendet")