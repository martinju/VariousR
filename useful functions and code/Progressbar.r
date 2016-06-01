
n.samp=2*10^4	
a=rep(0,n.samp)	


library(tcltk)
pb = tkProgressBar(title = "progress bar", min = 0,
                     max = n.samp, width = 500)

ptm=proc.time()

for (i in 1:n.samp)
	{
	Sys.sleep(0.00001)
	setTkProgressBar(pb, i-1, title=paste( round((i-1)/n.samp*100, 0),"% of total simulation done"))
	a=a+1
	}
	
	
timetime=(proc.time()-ptm)

close(pb)
print(timetime)
