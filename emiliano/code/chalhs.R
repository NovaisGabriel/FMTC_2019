# Latin hypercube sampling
# Guojun Gan
# Feb 11, 2019

############ 
# Generate sample
############
mylhs <- function(k, L, H, A) {
  # continuous part
  d1 <- length(L)
  mR <- matrix(runif(k*d1), nrow=k)
  mOrder <- apply(mR, 2, order)
  mN <- matrix(L, nrow=k, ncol=d1, byrow=T) + (mOrder - 1) * matrix(H-L, nrow=k, ncol=d1, byrow=T) / (k-1)

  # categorical part
  d2 <- length(A)
  mC <- matrix(0, nrow=k, ncol=d2)
  for(j in 1:d2) {
    mC[,j] <- sample(1:A[j], k, replace=T)
  }
  return(cbind(mN,mC))
}

L <- c(1,10)
H <- c(10,20)
A <- c(2, 5)
set.seed(1)
m1 <- mylhs(20,L,H,A)
table(m1[,3])
table(m1[,4])

set.seed(2)
m2 <- mylhs(20,L,H,A)
table(m2[,3])
table(m2[,4])

dev.new(width=8,height=4)
par(mfrow=c(1,2),mar=c(4,4,1,1))
plot(m1[,1],m1[,2])
plot(m2[,1],m2[,2])

############ 
# Calculate score
############
calDist <- function(mS, L, H) {
  k <- nrow(mS)
  d1 <- length(L)
  d2 <- ncol(mS) - d1

  mD <- matrix(0, nrow=k, ncol=k)  
  mN <- mS[,1:d1]    # continuous part
  mC <- mS[,-(1:d1)] # categorical part
  for(j in 1:k) {
    mD[,j] <- abs(mN - matrix(mN[j,], nrow=k, ncol=d1, byrow=T)) %*% 
      matrix( 1/(H-L), ncol=1) + 
      apply(sign(abs( mC - matrix(mC[j,], nrow=k, ncol=d2, byrow=T) )), 1, sum)
  }
  
  return(mD)
}

mD1 <- calDist(m1, L, H)
mD2 <- calDist(m2, L, H)
mD1[1:5,1:5]
mD2[1:5,1:5]

calScore <- function(mD) {
  ind <- lower.tri(mD)
  return(min(mD[ind]))
}

calScore(mD1)
calScore(mD2)


############ 
# Select representative policies
############

# Load data
setwd("C:/Users/Guojun Gan/Downloads/vamc/datasets")
setwd("/Volumes/Scratch/gan/MathDepartment/Data/vamc/datasets")
inforce <- read.csv("inforce.csv")
summary(inforce[,1:10])

vNames <- c("gbAmt", "gmwbBalance", "withdrawal",  paste("FundValue", 1:10, sep=""))

age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)

datN <- cbind(inforce[,vNames], data.frame(age=age, ttm=ttm))
datC <- inforce[,c("gender", "productType")]
datM <- as.matrix(cbind(datN, data.frame(lapply(datC, as.numeric))))

summary(datN)
summary(datC)

# Get input space
L <- apply(datN, 2, min)
H <- apply(datN, 2, max)
A <- sapply(lapply(datC, levels), length)
round(L,2)
round(H,2)
A

# Generate samples
k <- 340
nSamples <- 50
vScore <- c()
maxScore <- 0
bestS <- NULL
set.seed(123)
for(i in 1:nSamples) {
  mS <- mylhs(k,L,H,A)
  mD <- calDist(mS, L, H)
  score <- calScore(mD)
  if(score > maxScore) {
    maxScore <- score
    bestS <- mS
  }
  vScore <- c(vScore, score)
}
vScore
maxScore
head(bestS)

d1 <- length(L)
colnames(bestS) <- c(names(datN), names(datC))
summary(bestS[,1:d1])
table(bestS[,d1+1])
table(bestS[,d1+2])

plot(bestS[,1], bestS[,2])

# find nearest policies
findPolicy <- function(mS, dat, L, H) {
  ind <- c()
  k <- nrow(mS)
  n <- nrow(dat)
  d1 <- length(L)
  d2 <- ncol(dat) - d1

  for(i in 1:k) {
    mN <- dat[,1:d1]    # continuous part
    mC <- dat[,-(1:d1)] # categorical part

    vD <- abs(mN - matrix(mS[i,1:d1], nrow=n, ncol=d1, byrow=T)) %*% 
      matrix( 1/(H-L), ncol=1) + 
      apply(sign(abs( mC - matrix(mS[i,-(1:d1)], nrow=n, ncol=d2, byrow=T) )), 1, sum)
    tmp <- setdiff(order(vD), ind)
    ind <- c(ind, tmp[1])
  }

  return(sort(ind))
}

{
t1 <- proc.time()
lhs <- findPolicy(bestS, datM, L, H)
proc.time() - t1
}
lhs[1:50]
summary(datN[lhs,])
table(datC[lhs,])

pairs(datN[lhs,])

write.table(lhs, "lhs.csv", sep=",", quote=F, col.names=F)

# generate 680 reprsentative policies
k <- 680
nSamples <- 50
vScore <- c()
maxScore <- 0
bestS <- NULL
set.seed(123)
for(i in 1:nSamples) {
  mS <- mylhs(k,L,H,A)
  mD <- calDist(mS, L, H)
  score <- calScore(mD)
  if(score > maxScore) {
    maxScore <- score
    bestS <- mS
  }
  vScore <- c(vScore, score)
}
vScore
maxScore
head(bestS)

{
t1 <- proc.time()
lhs <- findPolicy(bestS, datM, L, H)
proc.time() - t1
}
lhs[1:50]
summary(datN[lhs,])
table(datC[lhs,])
write.table(lhs, "lhs2.csv", sep=",", quote=F, col.names=F)


