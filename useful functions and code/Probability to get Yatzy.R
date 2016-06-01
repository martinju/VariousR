
a=2.639
b=2.655

c(a+0.5*(b-a),(b-a)/2)



### Yatzy

n=10^5

throw=function(tt)
	{
	this.max=which.max(tt)
	this.number=as.numeric(names(tt)[this.max])
	many.numbers=tt[this.max]
	samp=sample(1:6,5-many.numbers,replace=TRUE)
	samp.new=c(samp,rep(this.number,many.numbers))
	return(table(samp.new))
	}

count=rep(0,n)
for (i in 1:n)
	{
	one=sample(1:6,5,replace=TRUE)
	tt.one=table(one)
	if (length(tt.one)==1)
		{
		count[i]=1
		}
	else	{
		tt.two=throw(tt.one)
		if (length(tt.two)==1)
			{
			count[i]=1
			}
		else	{
			tt.three=throw(tt.two)
			if (length(tt.three)==1)
				{
				count[i]=1
				}
			}
		}
	plot(1:i,cumsum(count[1:i])/(1:i),type='l')
	lines(c(-10,10^7),c(0.046,0.046),col=2)
#	print(c(i,count,count/i))
	}


#> sum(count)/n
#[1] 0.04472

# It is actually 0.06 according to Wikipedia.

