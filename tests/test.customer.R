
print("Starte Klassentest HNUCustomer")
tryCatch(
	{
		print("Test 1:  Erzeuge ein Objekt vom Typ HNUCustomer.")
		print(" 		Dieser Test sollte keine Fehler prodzuieren.")
		k1 <- new("HNUCustomer", x=12, y=10, label="myNode", id="N0001", demand = 30)
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
# should work fine

tryCatch(
	{
		print("Test 2:  Erzeuge ein Objekt vom Typ HNUCustomer.")
		print(" 		Dieser Test sollte einen Fehler prodzuieren, da die Initialisierung des Objektes falsch ist.")
		
		k2 <- new("HNUCustomer", x=c(12,12), y=c(10,10), label="myNode.error", id="N000.Error", demand = 30)
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

tryCatch(
	{
		print("Test 3:  Erzeuge ein Objekt vom Typ HNUCustomer.")
		print(" 		Dieser Test sollte keine Fehler prodzuieren.")
		k3 <- new("HNUCustomer", x=12, y=10, label="myNode.error", id="N0001.error", demand = -30)
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
		print("Test 4:  Erzeuge ein Objekt vom Typ HNUNOde und dann eins vom Typ HNUCustomer.")
		print(" 		Dieser Test sollte keine Fehler prodzuieren.")
		obj1 <- new("HNUNode", x=12, y=10, label="myNode", id="N0001")
		k4 <- new("HNUCustomer", location=obj1, demand = 30)
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
 

print("Klassentest HNU NODE - beendet")
