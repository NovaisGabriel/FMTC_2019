# tree-based models

calMeasure <- function(y, yhat) {
  pe <- sum(yhat - y) / sum(y)
  r2 <- 1 - sum( (yhat-y)^2 )/sum( (y-mean(y))^2 )
  return(list(pe=pe, r2=r2))
}

require(rpart)
require(gbm)
require(randomForest)

setwd("C:/Users/Guojun Gan/Downloads/vamc/datasets")
setwd("/Volumes/Scratch/gan/MathDepartment/Data/vamc/datasets")
inforce <- read.csv("inforce.csv")

vNames <- c("gbAmt", "gmwbBalance", "withdrawal",  paste("FundValue", 1:10, sep=""))

age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)

datN <- cbind(inforce[,vNames], data.frame(age=age, ttm=ttm))
datC <- inforce[,c("gender", "productType")]

greek <- read.csv("Greek.csv")
greek <- greek[order(greek$recordID),]

dat <- cbind(datN, datC, fmv=greek$fmv/1000)
summary(dat)

# based on lhs, clhs, hkmeans
S <- read.table("hkmeans.csv", sep=",")
S <- S[,2]

{t1 <- proc.time()
set.seed(1)
tree1 <- rpart(fmv ~ ., data=dat[S,],  method = "anova")
yhat <- predict(tree1, dat)
proc.time()-t1}

calMeasure(greek$fmv/1000, yhat)

png("treekhmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(RT)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(RT)")
abline(0,1)
dev.off()

tree1summary <- summary(tree1)
vImp <- tree1summary$variable.importance
vImp <- vImp * 100 / max(vImp)
ind <- order(vImp)
dev.new(width=6, height=4)
par(las=2) # make label text perpendicular to axis
par(mar=c(3,6,1,1)) # increase y-axis margin.
barplot(vImp[ind], main="", beside=TRUE,horiz=TRUE, names.arg=names(vImp[ind]), cex.names=0.8)

# bagging
{t1 <- proc.time()
set.seed(1)
bag1 <- randomForest(formula=fmv ~ ., data=dat[S,], mtry=17, importance=TRUE)
yhat <- predict(bag1, dat)
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("baghkmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(Bagged)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(Bagged)")
abline(0,1)
dev.off()

# boosting
{t1 <- proc.time()
set.seed(1)
boost1 <- gbm(formula=fmv ~ ., data=dat[S,], distribution="gaussian", n.trees=1000, interaction.depth=3)
yhat <- predict(boost1, newdata=dat, n.trees=1000)
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("boosthkmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(Boosted)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(Bossted)")
abline(0,1)
dev.off()


# random forest
{t1 <- proc.time()
set.seed(1)
bag1 <- randomForest(formula=fmv ~ ., data=dat[S,], mtry=4, importance=TRUE)
yhat <- predict(bag1, dat)
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)


png("rfhkmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(Random Forest)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(Random Forest)")
abline(0,1)
dev.off()
