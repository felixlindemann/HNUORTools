
plotGeoSitatuon.bordersgermany <- function(...) UseMethod("plotGeoSitatuon.bordersgermany")
plotGeoSitatuon.bordersgermany.default<-function(...){
    li <- list(...)
    message("plotGeoSitatuon.bordersgermany\n")
   	bordersgermany.polygon <- NULL
   	cities <- NULL
	data(bordersgermany, envir = environment())
 	
 	if(is.null(bordersgermany.polygon)) stop("Polygons of Germany not found. Has the data 'bordersgermany' been loaded?")

 	if(is.null(cities)) stop("Cities of Germany not found. Has the data 'bordersgermany' been loaded?")

	if(is.null(li$xlim)) li$xlim<- c(0,620)
	if(is.null(li$ylim)) li$ylim<- c(0,850) 
	if(is.null(li$polygons.border.color)) li$polygons.border.color<- 1
	if(is.null(li$polygons.fill.color)) li$polygons.fill.color<- "gray98"


	if(is.null(li$plotPolygons)) li$plotPolygons <- 1:length(bordersgermany.polygon)
	if(is.null(li$plotCities))   li$plotCities <- TRUE

	plot(NA, NA, xlim = li$xlim, ylim = li$ylim, asp=1, ...)

	for(i in li$plotPolygons){
		l<-bordersgermany.polygon[[i]]  
		for(j in 1:length(l)){ 
			l.c <-l[[j]]

			#lines(l.c[,1], l.c[,2], ...) 
			polygon(c(l.c[,1],l.c[1,1]), c(l.c[,2],l.c[1,2]), 
				    border = li$polygons.border.color , col = li$polygons.fill.color )
		} 
	} 
	if(li$plotCities)
		points(cities$x, cities$y, ...)
}