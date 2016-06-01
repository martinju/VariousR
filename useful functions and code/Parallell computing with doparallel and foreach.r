#### Testing paralell computing in R with doParallel and foreach

#install.packages("doParallel")

library(doParallel)

# A test of simple for loop:

#number of iterations in the loop
iters<-500
 
#vector for appending output
ls<-vector('list',length=iters)
 
#start time
strt<-Sys.time()
 
#loop
for(i in 1:iters){
 
    #counter
    cat(i,'\n')
 
    to.ls<-rnorm(1e6)
    to.ls<-summary(to.ls)
     
    #export
    ls[[i]]<-to.ls
         
    }
 
#end time
print(Sys.time()-strt)
# Time difference of 2.944168 secs


#import packages
library(foreach)
library(doParallel)
#library(doSNOW)

#install.packages("doSNOW")

#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)
#registerDoSNOW(cl)

#start time
iters<-100

strt<-Sys.time()
#loop
ls<-foreach(i=1:iters) %dopar% {
    okok<-rnorm(1e6,mean=i)
    okok<-summary(okok)
    okok
    }
 
print(Sys.time()-strt)
stopCluster(cl)







