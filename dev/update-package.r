setwd("/Volumes/Daten/FelixLindemann/Documents/git/")
 
# update package
library(devtools)

check("HNU-OR-Tools-For-R-Project")

build("HNU-OR-Tools-For-R-Project")

install("HNU-OR-Tools-For-R-Project")

library("HNUORToolsForRProject")
