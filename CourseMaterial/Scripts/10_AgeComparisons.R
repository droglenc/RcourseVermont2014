
library(FSA)      # for ageBias(), agePrecision()
setwd("C:/aaaWork/Web/fishR/courses/Vermont2014/CourseMaterial/")

d <- read.csv("Data/AlewifeLH.csv",header=TRUE)
str(d)
ab1 <- ageBias(otoliths~scales,data=d,col.lab="Otolith Age",row.lab="Scale Age")
summary(ab1,what="symmetry",flip.table=TRUE)
summary(ab1,what="bias")
plot(ab1)                  # LEFT
plot(ab1,difference=TRUE)  # RIGHT

sb <- read.csv("Data/StripedBass4.csv",header=TRUE)
str(sb)
ap1 <- agePrecision(reader1~reader2,data=sb)
summary(ap1,what="agreement")
summary(ap1,what="precision")

ab2 <- ageBias(reader1~reader2,data=sb,col.lab="Reader 1",row.lab="Reader 2")
summary(ab2,what="symmetry",flip.table=TRUE)
plot(ab2)    # Left
plot(ab2,difference=TRUE,ylim=c(-1.2,1))   # Right

