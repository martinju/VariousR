
# Creating a 2 dimensional heat plot with black and white 
library(hexbin)
aa=rnorm(10000)

bin<-hexbin(aa,aa+rnorm(10000,0,0.5), xbins=50) 
plot(bin)
