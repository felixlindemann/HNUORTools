
	bl <- c("Sachsen", "Brandenburg", "Berlin", "Bayern" , "Baden-Wuertemberg" , "Rheinland-Pfalz",
		"Saarland", "Hessen", "Thueringen", "Nord-Rhein-Westfahlen", "Sachsen-Anhalt", "Niedersachsen",
		"Bremen", "Hamburg", "MkPom", "Ruegen", "Perlworm", "ost1", "ost2", "ost3", "ost4", "ost5", "ost6", "ost7", "Helgoland", "Foehr", "Nordstrand", "Ns2", "Schlesweig-Holstein", "Sylt", "Sylt2"

	)

	bl.index <- c(1:15)
bordersgermany.labels <- bl[bl.index]
	li<- list()

for(i in bl.index){
	li[[length(li)+1]] <- list(m1=bordersgermany[[bl.index[i]]]*145.45/2*1.041383)
}
li[[length(li)+1]] <- list()

	nds<- c(17:25) # 	12
	mkpom <- c(16) #15
	schleswig <- c(14,26:31 ) #29 --> neu 16

j<- 12
	for( i in nds){

		x <-  li [[j]]
		x[[length(x)+1]] <- bordersgermany[[i]]*145.45/2*1.041383
		li[[j]] <-x

	}

j<- 15
	for( i in mkpom){

		x <-  li [[j]]
		x[[length(x)+1]] <- bordersgermany[[i]]*145.45/2*1.041383
		li[[j]] <-x

	}
j<- 16
	for( i in schleswig){

		x <-  li [[j]]
		x[[length(x)+1]] <- bordersgermany[[i]]*145.45/2*1.041383
		li[[j]] <-x

	}
 
bordersgermany.polygon <- li 
 
ds<-c("bordersgermany.polygon", "bordersgermany.labels", "cities")

save(list=ds, file=paste("data/bordersgermany.rda", sep=""))
load_all()

plotGeoSitatuon.bordersgermany(pch=20, cex=1)
