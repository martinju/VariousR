# Define minutes to plot on y axis

min.xx=seq(180,260,by=20)
min.yy=seq(500,600,by=20)



# convert to hours and mintes (helt sikkert en r-funksjon som gjør dette rett fram)
hours.xx=floor(min.xx/60)
restmin.xx=min.xx-hours.xx*60
h.m.labels.xx=paste(hours.xx,":",restmin.xx,sep="")
h.m.labels.xx[restmin.xx==0]=paste(h.m.labels.xx[restmin.xx==0],"0",sep="")	#adding extra zero if necessary

hours.yy=floor(min.yy/60)
restmin.yy=min.yy-hours.yy*60
h.m.labels.yy=paste(hours.yy,":",restmin.yy,sep="")
h.m.labels.yy[restmin.yy==0]=paste(h.m.labels.yy[restmin.yy==0],"0",sep="")	#adding extra zero if necessary



# define test function
xx <- runif(100, 180,260)
yy <- runif(100,500,600) 


# Plotter 
plot(xx,yy,axes=F,ylim=range(min.yy),xlim=range(min.xx))
axis(side=1,at=min.xx,labels=h.m.labels.xx)
axis(side=2,at=min.yy,labels=h.m.labels.yy)

