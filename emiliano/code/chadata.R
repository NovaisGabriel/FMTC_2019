# data description
# Feb 10, 2019
# Guojun Gan

setwd("C:/Users/Guojun Gan/Downloads/vamc/datasets")
inforce <- read.csv("inforce.csv")
summary(inforce)

with(inforce, table(productType,gender))

vNames <- c("gbAmt", "gmwbBalance", "withdrawal", 
  paste("FundValue", 1:10, sep=""))

age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)

dat <- cbind(inforce[,vNames], data.frame(age=age, ttm=ttm))
summary(dat)

mSum <- matrix(0, nrow=ncol(dat), ncol=5)
rownames(mSum) <- names(dat)
colnames(mSum) <- c("Min", "1st Q", "Mean", "3rd Q", "Max")

mSum[,1] <- apply(dat, 2, min) 
mSum[,2] <- apply(dat, 2, quantile, 0.25) 
mSum[,3] <- apply(dat, 2, mean) 
mSum[,4] <- apply(dat, 2, quantile, 0.75) 
mSum[,5] <- apply(dat, 2, max) 

round(mSum, 2)

greek <- read.csv("Greek.csv")
greek <- greek[order(greek$recordID),]

# histograms
dev.new(width=6, height=6)
par(mfrow=c(3,2), mar=c(4,4,1,1))
for(i in 2:7) {
   hist(greek[,i], br=100, main="", xlab=names(greek)[i])
}

dev.new(width=6, height=8)
par(mfrow=c(4,2), mar=c(4,4,1,1))
for(i in 8:15) {
   hist(greek[,i], br=100, main="", xlab=names(greek)[i])
}

mSum <- matrix(0, nrow=ncol(greek)-1, ncol=5)
rownames(mSum) <- names(greek)[-1]
colnames(mSum) <- c("Min", "1st Q", "Mean", "3rd Q", "Max")

mSum[,1] <- apply(greek[,-1], 2, min) 
mSum[,2] <- apply(greek[,-1], 2, quantile, 0.25) 
mSum[,3] <- apply(greek[,-1], 2, mean) 
mSum[,4] <- apply(greek[,-1], 2, quantile, 0.75) 
mSum[,5] <- apply(greek[,-1], 2, max) 

round(mSum, 2)



