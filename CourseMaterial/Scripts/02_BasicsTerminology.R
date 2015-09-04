
library(FSA)     # for mrClosed

3+4*2            # this is an expression
res <- 3+4*2     # but this is an assignment
res              #   to see what was assigned to memory
( res <- 9+3*6 ) # assign AND view

sqrt(17)
( res1 <- sqrt(17) )

dat <- c(3,6,8,3,5,6,2,7,6,8,2,10)
mean(dat)
mean(dat,trim=0.1)

mr1 <- mrClosed(M=346,n=184,m=49,type="Chapman")
summary(mr1)
confint(mr1)

( lake <- c("Star","Twin","Long","Deep") )
( numSpec <- c(4,8,7,3) )
( maxDepth <- c(6.5,7.8,3.8,25.6) )
( springFed <- c(TRUE,FALSE,FALSE,TRUE) )

lake[1]
lake[2]
lake[c(1,2)]
lake[-1]
lake[c(TRUE,FALSE,FALSE,TRUE)]
lake=="Star"
maxDepth[lake=="Star"]
numSpec[maxDepth<7]

# Put previous vectors into a data.frame.  For realistic sizes of data sets
#   I would enter data externally and read into R ... more on this later
( df <- data.frame(lake,numSpec,maxDepth,springFed) )
df[1,1]
df[1,]
df[c(1,2),]
df[-1,]
df[,2]
df[,"numSpec"]

str(df)
df$numSpec
df$numSpec[1]
mean(df$numSpec)

