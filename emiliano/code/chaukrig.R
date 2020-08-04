# universal kriging

ukrig <- function(Z, FZ, y, X, FX, varmodel) {
  # Perform ordinary kriging prediction
  #
  # Args:
  #   Z: a kxd matrix
  #   FZ: a kxJ matrix to capture trend for Z
  #   y: a vector of length k
  #   X: a nxd matrix
  #   FX: a nxJ matrix to capture trend for X
  #   varmodel: a variogram model
  #
  # Returns:
  #   a vector of predicted values for X

  k <- nrow(Z)
  n <- nrow(X)
  d <- ncol(Z)
  J <- ncol(FZ)
  
  # calculate distance matrix for Z
  hZ <- matrix(0, nrow=k, ncol=k)
  for(i in 1:k) {
    hZ[i,] <- (apply((Z - matrix(Z[i,], nrow=k, ncol=d, byrow=T))^2, 1, sum))^0.5
  }

  # calculate distance matrix between Z and X
  hD <- matrix(0, nrow=k, ncol=n)
  for(i in 1:k) {
    hD[i,] <- (apply((X - matrix(Z[i,], nrow=n, ncol=d, byrow=T))^2, 1, sum))^0.5
  }
  
  # construct kriging equation system
  V <- matrix(0, nrow=k+J, ncol=k+J)
  V[1:k, 1:k] <- varmodel(hZ)
  V[1:k, (k+1):(k+J)] <- FZ
  V[(k+1):(k+J), 1:k] <- t(FZ)

  D <- matrix(1, nrow=k+J, ncol=n)
  D[1:k,] <- varmodel(hD)
  D[(k+1):(k+J),] <- t(FX)

  # solve equation
  mW <- solve(V, D)
  
  # perform prediction
  mY <- matrix(0, nrow=k+J, ncol=1)
  mY[1:k,1] <- y
  yhat <- t(mW) %*% mY
  
  return(yhat)
}

expVM <- function(h, a, b, c) {
  res <- h
  ind <- h>0
  res[!ind] <- 0
  res[ind] <- b + c*(1-exp(-3*h[ind]/a)) 
  return(res)
}

sphVM <- function(h, a, b, c) {
  res <- h
  ind <- h==0
  res[ind] <- 0 
  ind <- h>0 & h<= a
  res[ind] <- b + c*(1.5*h[ind]/a-0.5*(h[ind]/a)^3) 
  ind <- h > a
  res[ind] <- b + c
  return(res)
}

gauVM <- function(h, a, b, c) {
  res <- h
  ind <- h>0
  res[!ind] <- 0
  res[ind] <- b + c*(1-exp(-3*(h[ind]/a)^2))
  return(res)
}

fitVarModel <- function(Z, y, vm, method) {
  # fit a variogram model to data
  #
  # args:
  #  Z: a kxd design matrix
  #  FZ: a kxJ matrix to capture trend
  #  yo: a vector of length k
  #  vm: a variogram function 
  #  method: fitting method ("default" or an integer specifying bins)
  #
  # returns:
  #   c(a,b,c) parameters of a variogram model
  
  # calculate distance matrix for Z
  k <- nrow(Z)
  d <- ncol(Z)
  hZ <- matrix(0, nrow=k, ncol=k)
  for(i in 1:k) {
    hZ[i,] <- (apply((Z - matrix(Z[i,], nrow=k, ncol=d, byrow=T))^2, 1, sum))^0.5
  }
  vD <- hZ[upper.tri(hZ)]
  da <- quantile(vD, 0.95)
  db <- 0
  dc <- var(y)
   
  if(method == "default") {
  	return(c(da, db, dc))
  }

  nBin <- method
  if(nBin <=3) {
    stop("number of bins <=3")
  }
 
  dMin <- min(vD)
  dMax <- max(vD) + 1
  dBandWidth <- (dMax - dMin) / nBin
  vh <- c()
  vy <- c()
  for(j in 1:nBin) {
    dL <- dMin + (j-1)* dBandWidth
    dU <- dL + dBandWidth
    ind <- which(vD >= dL & vD < dU)
    if(length(ind) > 0) {
      dSum <- 0
      for(t in ind) {
        cInd <- ceiling(sqrt(2*t + 0.25)+0.5)
        rInd <- t - (cInd-1)*(cInd-2)/2
        dSum <- dSum + (y[rInd] - y[cInd])^2
      }
      vh <- c(vh, (dL + dU)/2)
      vy <- c(vy, dSum / (2 * length(ind)))
    }
  }

  plot(vh,vy,main="Fit variogram model", xlab="h", ylab="gamma(h)")
  fit <- nls(vy~vm(vh,a,b,c),start=list(a=da,b=db,c=dc))
  
  res <- coef(fit)
  # plot empirical and fitted variogram models
  curve(vm(x,res[1], res[2], res[3]),add=TRUE)

  return(res)
}

calMeasure <- function(y, yhat) {
  pe <- sum(yhat - y) / sum(y)
  r2 <- 1 - sum( (yhat-y)^2 )/sum( (y-mean(y))^2 )
  return(list(pe=pe, r2=r2))
}

setwd("C:/Users/Guojun Gan/Downloads/vamc/datasets")
setwd("/Volumes/Scratch/gan/MathDepartment/Data/vamc/datasets")
inforce <- read.csv("inforce.csv")

vNames <- c("gbAmt", "gmwbBalance", "withdrawal",  paste("FundValue", 1:10, sep=""))

age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)

datN <- cbind(inforce[,vNames], data.frame(age=age, ttm=ttm))
datC <- inforce[,c("gender", "productType")]
dat <- cbind(datN, datC)

greek <- read.csv("Greek.csv")
greek <- greek[order(greek$recordID),]

# prepare data
X <- model.matrix( ~ ., data=dat)[,-1]
colnames(X)

vMin <- apply(X, 2, min)
vMax <- apply(X, 2, max)
X <- (X - matrix(vMin, nrow=nrow(X), ncol= ncol(X), byrow=TRUE)) / matrix(vMax-vMin, nrow=nrow(X), ncol= ncol(X), byrow=TRUE)

FX <- cbind(1, X)
head(FX, n=2)

# universal kriging based on LHS
S <- read.table("lhs.csv", sep=",")
S <- S[,2]
Z <- X[S,]
FZ <- FX[S,]
y <- greek$fmv[S]/1000

# fit variogram
{t1 <- proc.time()
res <- fitVarModel(Z, y, gauVM, 100)
proc.time()-t1}
res

{t1 <- proc.time()
yhat <- ukrig(Z, FZ, y, X, FX, function(h) {(gauVM(h, res[1], res[2], res[3]))})
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gaulhs.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(UK)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(UK)")
abline(0,1)
dev.off()

# universal kriging based on cLHS
S <- read.table("clhs.csv", sep=",")
S <- S[,2]
Z <- X[S,]
FZ <- FX[S,]
y <- greek$fmv[S]/1000

# fit variogram
{t1 <- proc.time()
res <- fitVarModel(Z, y, gauVM, 100)
proc.time()-t1}
res

{t1 <- proc.time()
yhat <- ukrig(Z, FZ, y, X, FX, function(h) {(gauVM(h, res[1], res[2], res[3]))})
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gauclhs.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(UK)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(UK)")
abline(0,1)
dev.off()

# universal kriging based on hkmenas
S <- read.table("hkmeans.csv", sep=",")
S <- S[,2]
Z <- X[S,]
FZ <- FX[S,]
y <- greek$fmv[S]/1000

# fit variogram
{t1 <- proc.time()
res <- fitVarModel(Z, y, gauVM, 100)
proc.time()-t1}
res

{t1 <- proc.time()
yhat <- ukrig(Z, FZ, y, X, FX, function(h) {(gauVM(h, res[1], res[2], res[3]))})
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gauhkmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(UK)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(UK)")
abline(0,1)
dev.off()


