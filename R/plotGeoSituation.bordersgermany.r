
plotGeoSitatuon.bordersgermany <- function(...) UseMethod("plotGeoSitatuon.bordersgermany")
plotGeoSitatuon.bordersgermany.default<-function(...){
    li <- list(...)
    message("plotGeoSitatuon.bordersgermany\n")
   
	data(bordersgermany)
 
	if(is.null(li$xlim)) li$xlim<- c(0,620)
	if(is.null(li$ylim)) li$ylim<- c(0,850)
	if(is.null(li$xlab)) li$xlab<- ""
	if(is.null(li$ylab)) li$ylab<- ""
	if(is.null(li$main)) li$main<- ""
	if(is.null(li$sub))  li$sub<- ""


	if(is.null(li$plotPolygons)) li$plotPolygons <- 1:length(bordersgermany.polygon)
	if(is.null(li$plotCities))   li$plotCities <- TRUE

	plot(NA, NA, xlim = li$xlim, ylim = li$ylim, xlab = li$xlab, ylab = li$ylab,
		main = li$main, sub = li$sub, asp=1)

	for(i in li$plotPolygons){
		l<-bordersgermany.polygon[[i]]  
		for(j in 1:length(l)){ 
			l.c <-l[[j]]

			#lines(l.c[,1], l.c[,2], ...) 
			polygon(c(l.c[,1],l.c[1,1]), c(l.c[,2],l.c[1,2]), 
				    border = 1, col = "gray98" )
		} 
	} 
	if(li$plotCities)
		points(cities$x, cities$y, ...)
}