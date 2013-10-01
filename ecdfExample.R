

alpha=.05
n=60
data = rnorm(n)
fhat = ecdf(data)
plot(fhat)

xgrid=seq(-4,4,length.out=101)
lines(xgrid,pnorm(xgrid))

xbar=mean(data)
s = sd(data)

lines(xgrid,pnorm(xgrid,xbar,s),col="blue",lwd=2)
rug(data)
ep = sqrt(log(2/alpha)/(2 * n))

lines(sort(data),(1:n)/n +ep ,type="s")
lines(sort(data),(1:n)/n - ep ,type="s")

