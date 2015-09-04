
library(FSA)     # for Subset(), view(), lencat()

setwd("C:/aaaWork/Web/fishR/courses/Vermont2014/CourseMaterial/") # Derek's computer only
d <- read.csv("Data/MNBCData.csv",header=TRUE)
str(d)
view(d)
nrow(d)

d[5,]
d[c(5,11,17),]
d$age[1:25]

levels(d$species)
levels(d$lake)

dBLC <- Subset(d,species=="BLC")
xtabs(~species,data=dBLC)
dBLCTL <- Subset(d,species=="BLC" & lake=="Talcot")
xtabs(~species+lake,data=dBLCTL)
dBLCBLG <- Subset(d,species=="BLC" | species=="BLG")
xtabs(~species,data=dBLCBLG)
d2 <- Subset(d,species!="BLC")
xtabs(~species,data=d2)
dPred <- Subset(d,species %in% c("LMB","NOP","SMB","WAE"))
xtabs(~species,data=dPred)
dgt500 <- Subset(d,lencap>=500)
nrow(dgt500)
min(dgt500$lencap)

d$lenin <- d$lencap/25.4
d$loglen <- log(d$lencap)
view(d)
# Create a year factor (categorical) variable
d$fyearcap <- factor(d$yearcap)
str(d)
levels(d$fyearcap)

# Create a length categorization variable
d <- lencat(~lencap,data=d,startcat=75,w=25)
view(d)
xtabs(~species+LCat,data=d)

# Focus hereafter on Talcot Lake Black Crappie in 2006 (only year sampled)
#   and eliminate several variables not used (for illustration & simplicity)
dBLC <- Subset(d,species=="BLC" & lake=="Talcot" & yearcap==2006,
               select=-c(gear,yearcap,lenin,loglen,fyearcap,LCat))
levels(dBLC$species)
levels(dBLC$lake)

# list the variables that contain the repeated measurements
varying1 <- c("anu1","anu2","anu3","anu4","anu5","anu6",
              "anu7","anu8","anu9","anu10","anu11","anu12")
# this is an alternative to the above that eliminates repetitive typing
( varying2 <- which(grepl("anu",names(dBLC))) )

# do the reshaping
ldBLC <- reshape(dBLC,direction="long",
                 idvar="fish",         # what identifies unique fish
                 varying=varying1,     # declare the repeated measurements
                 v.names="anu",        # name for repeat meas in long format
                 timevar="age",        # name of var that identifies the repeat
                 times=1:12)           # values in timevar for repeat

view(ldBLC)

ldBLC[ldBLC$fish==165,]                # example for one fish

# remove all of the NAs
ldBLC <- Subset(ldBLC,!is.na(anu))
ldBLC[ldBLC$fish==165,]                # same example for one fish

# remove the "plus" growth
ldBLC <- Subset(ldBLC,agecap-age>=0)
ldBLC[ldBLC$fish==165,]                # same example for one fish

k <- 35  # use Carlander intercept of k=35 mm
ldBLC <- within(ldBLC, {
  bcFL <- (anu/radcap)*(lencap-k)+k
})
view(ldBLC)
Summarize(bcFL~age,data=ldBLC)

