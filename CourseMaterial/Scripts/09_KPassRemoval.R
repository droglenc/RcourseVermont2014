
library(FSA)      # for removal()
setwd("C:/aaaWork/Web/fishR/courses/Vermont2014/CourseMaterial/") # Derek's Computer

d <- c(71,48,40)
res <- removal(d,type="CarleStrub")
summary(res)
confint(res)

d <- read.csv("Data/JonesStockwell2.csv",header=TRUE)
head(d)

( res <- apply(d[,4:6],MARGIN=1,FUN=removal,type="CarleStrub",just.ests=TRUE) )
# transpose the result and make as a data.frame, add specific info from d, add CIs
res <- data.frame(t(res))
res <- cbind(d[,1:3],res)
res <- within(res,{
  No.LCI <- No-1.96*No.se
  No.UCI <- No+1.96*No.se  
})
head(res)                 # first 6 rows

