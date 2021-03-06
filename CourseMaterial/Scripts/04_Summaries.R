
library(FSA)     # for Subset(), Summarize(), hist(), fact2num()
library(plotrix) # for plotCI()

setwd("C:/aaaWork/Web/fishR/courses/Vermont2014/CourseMaterial/") # Derek's Computer
d <- read.csv("Data/MnFats.csv",header=TRUE)
d <- Subset(d,sex!="UNK")  # removed one unknown sex individual (for simplicity)
str(d)
d <- within(d, {
            fyear <- factor(year)
            loglen <- log(len)
            logwt <- log(wt)
            } )
view(d)

Summarize(~age,data=d,digits=2)
hist(~age,data=d,xlab="Age (yrs)",breaks=seq(5,35,1))

( sextbl <- xtabs(~sex,data=d) )
prop.table(sextbl)*100
barplot(sextbl,xlab="Sex",ylab="Frequency")                    # Left
barplot(prop.table(sextbl)*100,xlab="sex",ylab="Percentage")   # Right

Summarize(age~sex,data=d,digits=2)
hist(age~sex,data=d,xlab="Age (yrs)",breaks=seq(5,35,1),nrow=2,ncol=1)

boxplot(age~sex,data=d,xlab="Sex",ylab="Age (yrs)",col="gray90",notch=TRUE)

histStack(age~sex,data=d,xlab="Age (yrs)",breaks=seq(5,35,1),ylim=c(0,60),
          col="gray.colors",legend="topright")

agesextbl <- xtabs(~sex+age,data=d)
round(prop.table(agesextbl)*100,1)
round(prop.table(agesextbl,margin=1)*100,1)

plot(wt~len,data=d,xlab="Total Length",ylab="Weight",pch=16,col=rgb(0,0,0,1/8))

colM <- rgb(0,0,0,1/3)
colF <- rgb(1,0,0,1/3)
plot(logwt~loglen,data=Subset(d,sex=="M"),pch=16,col=colM,
     xlab="log Total Length",ylab="log Weight")
points(logwt~loglen,data=Subset(d,sex=="F"),pch=16,col=colF)
legend("topleft",c("Male","Female"),pch=16,bty="n",col=c(colM,colF))

lenAtAge <- Summarize(len~age,data=d,digits=1)
str(lenAtAge)
plot(mean~age,data=lenAtAge)  # NO GOOD!!
plot(mean~fact2num(age),data=lenAtAge,pch=16,xlab="Age",ylab="Mean TL")  # GOOD!!

lenAtAge <- within(lenAtAge, {
  LCI <- mean-1.96*sd/sqrt(n)
  UCI <- mean+1.96*sd/sqrt(n)
})
head(lenAtAge)
with(lenAtAge,plotCI(fact2num(age),mean,ui=UCI,li=LCI,
     pch=16,xlab="Age",ylab="Mean TL"))

