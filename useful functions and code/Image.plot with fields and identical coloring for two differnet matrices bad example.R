# Image plot with identical coloring for two different matrices

library(fields)
# Terrible example though

Sigma.eps.adjusted.C_2.val.old=matrix(rnorm(10^4,sd=0.001),ncol=100,nrow=100)
Sigma.eps.adjusted.C_2.val=Sigma.eps.adjusted.C_2.val.old+diag(rnorm(10^2,sd=0.005))
	
	
	breaks0=seq(-0.005,0.01,0.0001)
	nlevel0=length(breaks0)-1
	tim.colors0=tim.colors(nlevel0)
	labels0=seq(-0.005,0.01,0.001)
	
	image.plot(Sigma.eps.adjusted.C_2.val,breaks=breaks0, nlevel=nlevel0,col=tim.colors0,axis.args=list(at=labels0,labels=labels0))
	x11()
	image.plot(Sigma.eps.adjusted.C_2.val.old,breaks=breaks0, nlevel=nlevel0,col=tim.colors0,axis.args=list(at=labels0,labels=labels0))
