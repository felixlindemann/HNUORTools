


#54 265.44625 123.63250             Ulm             Ulm
# 19 247.26500 807.24750       Flensburg       Flensburg
#25 276.35500 676.34250         Hamburg         Hamburg
#26 254.53750 549.07375        Hannover        Hannover
#23 269.08250 458.16750       Gottingen       Gottingen
#32 236.35625 432.71375          Kassel          Kassel
#57 269.08250 272.71875        Wurzburg        Wurzburg
#2  330.89875 116.36000        Augsburg        Augsburg
#42 381.80625  98.17875         Munchen         Munchen
#52 218.17500 163.63125       Stuttgart       Stuttgart
#31 156.35875 192.72125       Karlsruhe       Karlsruhe
#34  69.08875 407.26000            Koln            Koln
#20 181.81250 305.44500       Frankfurt       Frankfurt
#44 349.08000 236.35625        Nurnberg        Nurnberg
#14 523.62000 410.89625         Dresden         Dresden
#4  494.53000 567.25500          Berlin          Berlin
#7  312.71750 534.52875    Braunschweig    Braunschweig
#13 105.45125 461.80375        Dortmund        Dortmund

delta <-NULL

m<-matrix(c(
	54, 19, 710, # ulm - Flensburg :710
	54, 25, 573, # Ulm - Hamburg  : 573
	54, 26, 442,# Ulm - Hannover  : 442
	54, 23, 348,# Ulm - Göttingen: 348
	54, 32, 326,# Ulm - Kassel : 326
	54, 57, 155,# Ulm - Würzburg: 155
	54, 2,  66 ,# Ulm - Augsburg. 66
	54, 42, 120 ,# Ulm - MUC: 120
	54, 52,  73 ,# Ulm Stuttgart: 73
	54, 31, 135 ,# Ulm - Karlsruhe 135
    34,20,152, # Köln Frankfurt: 152
	44,14,259, # nürnberg - Dresden 259
	44,42,154, # nürnberg - München 154
	 4,14,165, # Berlin Dresden 191
	 4,7,197, # Berlin - Braunschweig 197
	7,26,55, # BS - H 55
	 26,13,182, # H - Dortmund 182
	 32,13,142 # Kassel - Dortmund 142
), ncol=3, byrow=TRUE)

for( i in 1:nrow(m)){
	 delta[i] <-   m[i,3]-sqrt((diff(cities[m[i,1:2],1]))^2+(diff(cities[m[i,1:2],2]))^2) 
}
hist(delta)
 #cities[,1:2] <- cities[,1:2] *mean(delta)


