# R script for preliminary work on the interaction terms
# Case study on Variable Annuities

getwd()
# reset the working directory
setwd("D:/ConferencesWorkshops-2019/FinMathComp-Brazil2019/R code - Brazil 2019")
inforce <- read.csv("inforce.csv")
greek <- read.csv("Greek.csv")
# define two new variables in inforce
age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)
inforce <- cbind(inforce, data.frame(age=age, ttm=ttm))
# merge data frames
inforce2 <- merge(inforce, greek, 
                  by.x="recordID", by.y="recordID")
head(inforce2)
names(inforce2)
# pick variables, remove those with identical values
# remove issueDate and matDate, add ttm (time to maturity)
vNames <- c("recordID", "gender", "productType",
            "age", "ttm", "gbAmt", "gmwbBalance",
            "withdrawal", paste("FundValue", 1:10, sep=""), "fmv")
dat.new <- inforce2[,vNames]
names(dat.new)
# note the size of our dataset, 190k observations
dim(dat.new)
summary(dat.new)
attach(dat.new)
write.csv(dat.new,'datnew.csv',row.names=FALSE)

# analyze the data before performing transformation
summary(dat.new$fmv)
hist(dat.new$fmv, breaks=50, main = "Histogram of fmv", xlab = "fmv")
names(dat.new)
par(mfrow=c(4,3))
for(i in 9:18) {
  hist(dat.new[,i], breaks=30, main=names(dat.new)[i], xlab=NULL)
  }
# do a histogram of the other continuous variables (including fmv)
dev.off()
par(mfrow=c(3,2))
for(i in c(4:8)) {
  hist(dat.new[,i], breaks=30, main=names(dat.new)[i], xlab=NULL)
  }
# do a boxplot of the categorical variables
plot(fmv~gender)

# read the representative policies
samp1 <- read.csv("hkmeans.csv",header= FALSE)
samp2 <- read.csv("hkmeans2.csv",header= FALSE)
dim(samp1)
dim(samp2)
head(samp1)
head(samp2)

# representative policies to be used for training
