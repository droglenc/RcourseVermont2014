
library(FSA)      # for Subset(), fitPlot()

ages <- 9:12
yc67 <- c(1560,1031,445,300)
yc64 <- c(1116,571,342,156)
yc57 <- c(172,55,18,0.5)
yc54 <- c(192,63,39,5)
d <- data.frame(yc=factor(rep(c(1967,1964,1957,1954),each=4)),
                age=rep(ages,times=4),
                cpe=c(yc67,yc64,yc57,yc54))
d <- within(d,logcpe <- log(cpe))
d

# Compare 1957 and 1967 year-classes
lm1 <- lm(logcpe~age*yc,data=Subset(d,yc %in% c(1957,1967)))
anova(lm1)
summary(lm1)
confint(lm1)
fitPlot(lm1,legend="bottomleft")

# Compare 1964 and 1967 year-classes
lm2 <- lm(logcpe~age*yc,data=Subset(d,yc %in% c(1964,1967)))
anova(lm2)
summary(lm2)
confint(lm2)
fitPlot(lm2,legend="bottomleft")

# Fit without the insignificant interaction term as a demonstration
lm2a <- lm(logcpe~age+yc,data=Subset(d,yc %in% c(1964,1967)))
anova(lm2a)
summary(lm2a)
confint(lm2a)

