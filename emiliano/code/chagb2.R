# GB2 regression model

dGB2 <- function(x,a,b,gamma1,gamma2) {
  num <- abs(a)*(x^(a*gamma1-1))*(b^(a*gamma2))
  temp <- (b^a+x^a)^(gamma1+gamma2)
  den <- beta(gamma1,gamma2)*temp
  result <- num/den
  return(result)
}

eGB2 <- function(a, b, p, q) {
  return( b * beta(p + 1/a, q - 1/a) / beta(p, q) )
}

negllS3a <- function(param, y) {
  c <- -min(y) + 1e-6	
  beta0 <- log(mean(y + c)) 
  temp <- log(dGB2( y+ c, a= param[1], b=exp(beta0), gamma1= param[2], gamma2= param[3]))
  result <- -sum(temp) 
  if(is.nan(result) || abs(result) > 1e+10) {
    result = 1e+10
  }
  return(result)
}

negllS3b <- function(vp, y, param) {	
  c <- -min(y) + 1e-6	
  beta0 <- log(mean(y + c)) 
  temp <- log(dGB2( y+ vp, a= param[1], b=exp(beta0), gamma1= param[2], gamma2= param[3]))
  result <- -sum(temp) 
  if(is.nan(result) || abs(result) > 1e+10) {
    result = 1e+10
  }
  return(result)
}

negllS3c <- function(vp, X, y, param1) {	
  param <- c(param1, vp)
  xbeta <- X %*% as.matrix(param[-c(1:4)], ncol=1)

  temp <- log(dGB2( y+ param[4], a= param[1], b=exp(xbeta), gamma1= param[2], gamma2= param[3]))
  result <- -sum(temp) 
  if(is.nan(result) || abs(result) > 1e+10) {
    result = 1e+10
  }
  return(result)
}

negllS <- function(param, X, y) {
  xbeta <- X %*% as.matrix(param[-c(1:4)], ncol=1)
	
  temp <- log(dGB2( y+ param[4], a= param[1], b=exp(xbeta), gamma1= param[2], gamma2= param[3]))
  result <- -sum(temp) 
  if(is.nan(result) || abs(result) > 1e+10 ) {
    result = 1e+10
  }
  return(result)	
}

gb2 <- function(X, y, S) {
  # GB2 regression model
  #
  # args:
  #  X: nxd design matrix of the whole inforce
  #  y: a vector of k fair market values of representative policies
  #  S: a vector of k indices
  #
  # returns:
  #   a vector of predicted values

  L <- c(0, 0, 0)
  U <- c(10, 10,  10)
  NS <- 100
  SP <- matrix(0, nrow=NS, ncol=length(L))
  for(k in 1:length(L)) {
    SP[, k] <- runif(NS, min=L[k], max=U[k])
  }

  vLL <- matrix(0, nrow=NS, ncol=1)
  for(i in 1:NS) {
    vLL[i] <- negllS3a(SP[i,], y=y)
  }
  SP1 <- SP[order(vLL),]
  SP <- SP1[1:10,]

  # Stage 1
  mRes <- matrix(0, nrow=nrow(SP), ncol=2+length(L)) 
  for(i in 1:nrow(SP)) {
    fit.GB2 <- optim(SP[i,], negllS3a, NULL, y=y, control = list(maxit =10000))
    mRes[i, 1] <- fit.GB2$value
    mRes[i, 2] <- fit.GB2$convergence
    mRes[i, -c(1:2)] <- fit.GB2$par
  }

  # Stage 2
  iMin <- which.min(mRes[,1])
  ahat <- mRes[iMin, 3]
  phat <- mRes[iMin, 4]
  qhat <- mRes[iMin, 5]
  fit2 <- optimize(negllS3b, interval=c(-min(y)+1e-6, -10*min(y)), y=y, param=mRes[iMin, 3:5])
  chat <- fit2$minimum

  # Stage 3 
  fit3 <- optim(c(log(mean(y) + chat), rep(0, ncol(X)-1)), negllS3c, NULL, X=X[S,], y=y,  
    param1=c(mRes[iMin, 3:5], chat), control = list(maxit =50000))

  # Stage 4
  fit4 <- optim(c(mRes[iMin, 3:5], chat, fit3$par), negllS, NULL, X=X[S,], y=y, 
    control = list(maxit =50000))

  param.hat <- fit4$par
  a <- param.hat[1]
  p <- param.hat[2]
  q <- param.hat[3]
  c <- param.hat[4]
  b <- exp(X %*% as.matrix(param.hat[-c(1:4)], ncol=1))
  print(round(fit4$par,4))
  yhat <- eGB2(a, b, p, q) - c

  return(yhat)
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
X <- cbind(1, X) # add an intercept

# based on lhs
S <- read.table("lhs.csv", sep=",")
S <- S[,2]

y <- greek$fmv[S]/1000

{t1 <- proc.time()
set.seed(1)
yhat <- gb2(X, y, S)
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gb2lhs.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(GB2)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(GB2)")
abline(0,1)
dev.off()

# based on clhs
S <- read.table("clhs.csv", sep=",")
S <- S[,2]
y <- greek$fmv[S]/1000
{t1 <- proc.time()
set.seed(1)
yhat <- gb2(X, y, S)
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gb2clhs.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(GB2)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(GB2)")
abline(0,1)
dev.off()

# based on hkmeans
S <- read.table("hkmeans.csv", sep=",")
S <- S[,2]
y <- greek$fmv[S]/1000
{t1 <- proc.time()
set.seed(1)
yhat <- gb2(X, y, S)
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("gb2hkmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(GB2)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(GB2)")
abline(0,1)
dev.off()

