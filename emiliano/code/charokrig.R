# rank order kriging

okrig <- function(Z, y, X, varmodel) {
  # Perform ordinary kriging prediction
  #
  # Args:
  #   Z: a kxd matrix
  #   y: a vector of length k
  #   X: a nxd matrix
  #   varmodel: a variogram model
  #
  # Returns:
  #   a vector of predicted values for X

  k <- nrow(Z)
  n <- nrow(X)
  d <- ncol(Z)
  
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
  V <- matrix(1, nrow=k+1, ncol=k+1)
  V[k+1, k+1] <- 0
  V[1:k, 1:k] <- varmodel(hZ)

  D <- matrix(1, nrow=k+1, ncol=n)
  D[1:k,] <- varmodel(hD)

  # solve equation
  mW <- solve(V, D)
  
  # perform prediction
  mY <- matrix(0, nrow=k+1, ncol=1)
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
  #  y: a vector of length k
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

rokrig <- function(Z, y, X, varmodel) {
  # Perform rank ordinary kriging prediction
  #
  # Args:
  #   Z: a kxd matrix
  #   y: a vector of length k
  #   X: a nxd matrix
  #   varmodel: a variogram model
  #
  # Returns:
  #   a vector of predicted values for X

  # get standardized rank orders
  u <- rank(y) / length(y)
  
  # perform ordinary kriging
  uhat <- okrig(Z, u, X, varmodel)

  # back transformation
  require(Hmisc)
  uhat2 <- rank(uhat) / length(uhat)
  yhat <- approxExtrap(u, y, rule=2, xout=uhat2)
  mu <- mean(y)
  mue <- mean(yhat$y)
  yhat <- yhat$y * mu / mue
  
  return(yhat) 
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
summary(X[,1:10])

# ordinary kriging based on LHS
S <- read.table("lhs.csv", sep=",")
S <- S[,2]
Z <- X[S,]
y <- greek$fmv[S]/1000

# fit variogram
u <- rank(y) / length(y)
{t1 <- proc.time()
res <- fitVarModel(Z, u, gauVM, 100)
proc.time()-t1}
res

{t1 <- proc.time()
yhat <- rokrig(Z, y, X, function(h) {(gauVM(h, res[1], res[2], res[3]))})
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gaulhs.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(ROK)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(ROK)")
abline(0,1)
dev.off()

# ordinary kriging based on cLHS
S <- read.table("clhs.csv", sep=",")
S <- S[,2]
Z <- X[S,]
y <- greek$fmv[S]/1000

# fit variogram
u <- rank(y) / length(y)
{t1 <- proc.time()
res <- fitVarModel(Z, u, gauVM, 100)
proc.time()-t1}
res

{t1 <- proc.time()
yhat <- rokrig(Z, y, X, function(h) {(gauVM(h, res[1], res[2], res[3]))})
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gauclhs.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(ROK)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(ROK)")
abline(0,1)
dev.off()

# rank ordinary kriging based on hkmeans
S <- read.table("hkmeans.csv", sep=",")
S <- S[,2]
Z <- X[S,]
y <- greek$fmv[S]/1000

# fit variogram
u <- rank(y) / length(y)
{t1 <- proc.time()
res <- fitVarModel(Z, u, gauVM, 100)
proc.time()-t1}
res

{t1 <- proc.time()
yhat <- rokrig(Z, y, X, function(h) {(gauVM(h, res[1], res[2], res[3]))})
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gaukmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(ROK)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(ROK)")
abline(0,1)
dev.off()
