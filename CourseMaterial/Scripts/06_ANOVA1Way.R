
library(FSA)      # for Subset(), wrVal(), psdVal()  
library(car)      # for outlierTest(), leveneTest()
library(multcomp) # for glht(), mcp()
library(plotrix)  # for plotCI(), cld()

setwd("C:/aaaWork/Web/fishR/courses/Vermont2014/CourseMaterial/") # Derek's Computer
d <- read.csv("Data/Keuska99.csv",header=TRUE)
str(d)
levels(d$species)

# Focus on LMB and remove some variables (only to make the handout easier to read)
lmb <- Subset(d,species=="LMB",select=c("species","geartype","mm","grams"))

# Identify which fish had both mm and grams recorded, show first 10
complete.cases(lmb[,c("mm","grams")])[1:10]

# Retain only those with both measures
lmb <- Subset(lmb,complete.cases(lmb[,c("mm","grams")]))
str(lmb)
view(lmb)

( wsLMB <- wsVal("Largemouth Bass") )
lmb1 <- Subset(lmb,mm>=150)

lmb1 <- within(lmb1,{
          Ws <- 10^(wsLMB$int)*mm^wsLMB$slope
          Wr <- grams/Ws*100
        })
view(lmb1)

( wsPSD <- psdVal("Largemouth Bass") )
lmb1 <- lencat(~mm,data=lmb1,breaks=wsPSD)
view(lmb1)
xtabs(~LCat,data=lmb1)

lmb1 <- lencat(~mm,data=lmb1,breaks=c(0,200,300,380,1000))
view(lmb1)
xtabs(~LCat1,data=lmb1)

lm1 <- lm(Wr~LCat1,data=lmb1)
residPlot(lm1)
outlierTest(lm1)
lmb1[80,]                           # Outlier?
lmb1[lmb1$mm>=195 & lmb1$mm<=205,]  # Fish w/ similar lengths
lmb2 <- lmb1[-80,]                  # Remove the fish

lm2 <- lm(Wr~LCat1,data=lmb2)
leveneTest(lm2)
residPlot(lm2)        # Left
hist(~residuals(lm2)) # Right

anova(lm2)
mc1 <- glht(lm2,mcp(LCat1="Tukey"))
summary(mc1)
cld(mc1)
confint(mc1)

( sumWr <- Summarize(Wr~LCat1,data=lmb2,digits=1) )
sumWr <- within(sumWr, {
  LCI <- mean-1.96*sd/sqrt(n)
  UCI <- mean+1.96*sd/sqrt(n)
})
sumWr
with(sumWr,plotCI(1:4,mean,ui=UCI,li=LCI,pch=16,xlim=c(0.5,4.5),xaxt="n",
                  ylim=c(89,105),xlab="Length Category",ylab="Mean Wr"))
axis(1,1:4,c("Sub-Stock","Stock","Quality","Preferred"))
abline(h=100,col="red",lty=3,lwd=2)
text(1:4,sumWr$mean,c("ab","a","a","b"),pos=c(4,4,4,4))
text(1:4,89,paste("n=",sumWr$n,sep=""))

