
library(FSA)  # for mrClosed(), capHistSum()
setwd("C:/aaaWork/Web/fishR/courses/Vermont2014/CourseMaterial/") # Derek's Computer

bg <- read.csv("Data/BluegillJL.csv",header=TRUE)
view(bg)
bgtbl <- xtabs(~first+second,data=bg)
addmargins(bgtbl)
mr1 <- mrClosed(M=196,n=90,m=9,type="Chapman")
summary(mr1)
confint(mr1)

bgch <- capHistSum(bg)
bgch$caphist
mr2 <- mrClosed(bgch,type="Chapman")
summary(mr2)

mr3 <- mrClosed(M=c(70,200),n=c(52,172),m=c(9,37),type="Chapman",
                labels=c("550-659 mm",">=660 mm"))
summary(mr3,incl.SE=TRUE,incl.all=TRUE)
confint(mr3)

np <- read.csv("Data/PikeNYPartial1.csv",header=TRUE)
view(np)
npch <- capHistSum(np,cols=-1)
npch$caphist
npch$sum
mr4 <- mrClosed(npch,type="Schnabel")
summary(mr4)
confint(mr4)

mr5 <- mrClosed(n=c(16,19,16),m=c(0,7,7),R=c(16,19,0),type="Schnabel")
summary(mr5)
confint(mr5)

