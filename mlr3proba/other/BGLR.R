rm(list=ls())
setwd(tempdir())

#loading libraries
library(survival)
library(BGLR)

#loading data included in BGLR
data(wheat)

# simulation of data
X = wheat.X[,1:4]
n = nrow(X)
b = c(-2,2,-1,1)
error = rnorm(n)
y = X %*% b + error

cen = sample(1:n, size = 200)
non_cens = setdiff(1:n, cen)
yCen = y
yCen[cen] = NA
a=rep(NA,n)
b=rep(NA,n)
a[cen]=y[cen]-runif(min=0,max=1,n=200)
b[cen]=Inf

nIter=6000;
burnIn=1000;
thin=10;
saveAt='';
df0=5
S0=var(y)/2*(df0-2)
weights=NULL;
ETA=list(list(X=X,model='FIXED'))

# y = y or y = yCen?
fm1=BGLR(y=y,a=a,b=b,ETA=ETA,nIter=nIter,burnIn=burnIn,
         thin=thin,saveAt=saveAt,df0=df0,S0=S0,weights=weights)

head(fm1$yHat) # negative survival time

length(fm1$yHat)
rmse = sum((fm1$yHat[non_cens] - yCen[non_cens])^2) / length(non_cens)
rmse

predict(fm1)
# need the coefficients to get the linear predictor!
# predict on newdata?
fm1$

# fits the model using survreg
event=ifelse(is.na(yCen),0,1)
time=ifelse(is.na(yCen),a,yCen)

surv.object=Surv(time=time,event=event,type='right')
fm2=survreg(surv.object~X, dist="gaussian")

plot(fm1$ETA[[1]]$b~fm2$coeff[-1],pch=19,col=2,cex=1.5,
     xlab="survreg()", ylab="BGLR()")
abline(a=0,b=1,lty=2)
