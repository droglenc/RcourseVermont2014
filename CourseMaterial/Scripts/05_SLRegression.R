
library(FSA)     # for Subset(), residPlot(), fitPlot()

setwd("C:/aaaWork/Web/fishR/courses/Vermont2014/CourseMaterial/") # Derek's Computer
d <- read.csv("Data/MnFats.csv",header=TRUE)
d <- Subset(d,sex!="UNK")  # removed one unknown sex individual (for simplicity)
d <- within(d, { fyear <- factor(year)
                 loglen <- log(len)
                 logwt <- log(wt)
                } )
view(d)
clr <- rgb(0,0,0,1/4)

lm1 <- lm(wt~len,data=d)
names(lm1)
coef(lm1)
residuals(lm1)[1:10]   # only show first 10 residuals
fitted(lm1)[1:10]      # only show first 10 fitted values

plot(residuals(lm1)~fitted(lm1),pch=16,col=clr)  # manual construction -- Left
residPlot(lm1,col=clr)                           # auto construction -- Right

lm2 <- lm(logwt~loglen,data=d)
residPlot(lm2,col=clr)   # Left
hist(~residuals(lm2))    # Middle
fitPlot(lm2,col.pt=clr)  # Right

d[172,]                    # Outlier?
d[d$len>395 & d$len<405,]  # Fish w/ similar len
d1 <- d[-172,]             # Remove the fish

lm3 <- lm(logwt~loglen,data=d1)
anova(lm3)
summary(lm3)
coef(lm3)
confint(lm3)
# Predict weight for 400 mm individual
( p1 <- predict(lm3,data.frame(loglen=log(400)),interval="prediction") )
exp(p1)

plot(wt~len,data=d1,xlab="Total Length (mm)",ylab="Weight (g)",pch=16,col=clr)
( cf <- coef(lm3) )
curve(exp(cf[1])*x^cf[2],from=275,to=900,col="red",lwd=2,add=TRUE)

plot(wt~len,data=d1,xlab="Total Length (mm)",ylab="Weight (g)",pch=16,col=clr)
curve(exp(cf[1])*x^cf[2],from=275,to=900,col="red",lwd=3,add=TRUE)
( cfOut <- coef(lm2) )
curve(exp(cfOut[1])*x^cfOut[2],from=275,to=900,col="blue",lwd=1,add=TRUE)
legend("topleft",c("without Outlier","with Outlier"),col=c("red","blue"),lwd=2,bty="n")

# Predict weight for all lengths b/w 275 and 900 mm
xs <- seq(275,900,1)
pW <- exp(predict(lm3,data.frame(loglen=log(xs)),interval="prediction"))
pW[1:5,]  # first five rows
plot(wt~len,data=d1,xlab="Total Length (mm)",ylab="Weight (g)",pch=16,col=clr)
lines(pW[,"fit"]~xs,col="red",lwd=2)
lines(pW[,"lwr"]~xs,col="red",lty=2)
lines(pW[,"upr"]~xs,col="red",lty=2)

