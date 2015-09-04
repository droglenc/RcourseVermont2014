
library(FSA)      # for Subset(), vbModels(), vbStarts(), vbFuns(), confint()
library(nlstools) # for nlsBoot()

setwd("C:/aaaWork/Web/fishR/courses/Vermont2014/CourseMaterial/") # Derek's Computer
d <- read.csv("Data/TroutBR.csv",header=TRUE)
str(d)
rbt <- Subset(d,species=="Rainbow")
str(rbt)

# Declare some constants
xlbl <- "Age (yrs)"
ylbl <- "Total Length (in)"
clr <- rgb(0,0,0,1/20)

plot(tl~age,data=rbt,xlab=xlbl,ylab=ylbl,pch=16,col=clr)

vbModels()

( svb1 <- vbStarts(tl~age,data=rbt,type="typical") )
fit1 <- nls(tl~Linf*(1-exp(-K*(age-t0))),data=rbt,start=svb1)
summary(fit1)
confint(fit1)

( cf <- coef(fit1) )
plot(tl~age,data=rbt,xlab=xlbl,ylab=ylbl,pch=16,col=clr)
curve(cf["Linf"]*(1-exp(-cf["K"]*(x-cf["t0"]))),
      from=3,to=10,n=500,lwd=2,col="red",add=TRUE)

boot1 <- nlsBoot(fit1,niter=200)   # niter should be nearer 1000
ests1 <- boot1$coefboot
ests1[1:5,]  # first five rows
hist(~ests1[,"Linf"],xlab="Bootstrapped Linf Values")
hist(~ests1[,"K"],xlab="Bootstrapped K Values")

confint(boot1)

predict(fit1, data.frame(age=8))
pv <- ests1[,"Linf"]*(1-exp(-ests1[,"K"]*(8-ests1[,"t0"])))
hist(~pv,breaks=20,xlim=c(26.6,27.8),xlab="Bootstrapped Predicted Length at age-8")
quantile(pv,c(0.025,0.975))

ages <- c(3,9)
( vb2 <- vbFuns("Francis") )
( sv2 <- vbStarts(tl~age,data=rbt,type="Francis",tFrancis=ages) )
fit2 <- nls(tl~vb2(age,L1,L2,L3,t1=ages[1],t3=ages[2]),data=rbt,start=sv2)
summary(fit2)
plot(tl~age,data=rbt,xlab=xlbl,ylab=ylbl,pch=16,col=clr)
curve(vb2(x,L1=coef(fit2),t1=ages),from=3,to=10,n=500,lwd=2,col="red",add=TRUE)
boot2 <- nlsBoot(fit2,niter=200)   # niter should be nearer 1000
confint(boot2)

