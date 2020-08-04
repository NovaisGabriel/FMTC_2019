#### EMIL'S CODE: LINEAR MODEL WITH INTERECATIONS ####

# Packages #
require(glinternet)

# Auxiliary function from emiliano/codes/chaukrig.R
calMeasure <- function(y, yhat) {
    pe <- sum(yhat - y) / sum(y)
    r2 <- 1 - sum( (yhat-y)^2 )/sum( (y-mean(y))^2 )
    return(list(pe=pe, r2=r2))
}

# load data
inforce <- read.csv("data/raw/inforce.csv")

vNames <- c("gbAmt", "gmwbBalance", "withdrawal",  paste("FundValue", 1:10, sep=""))

age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)

datN <- cbind(inforce[,vNames], data.frame(age=age, ttm=ttm))
datC <- inforce[,c("gender", "productType")]

dat <- cbind(datN, gender=as.numeric(datC$gender)-1, 
             productType=as.numeric(datC$productType)-1)
summary(dat)

greek <- read.csv("data/raw/Greek.csv")
greek <- greek[order(greek$recordID),]

# based on hkmeans
S <- read.table("data/raw/hkmeans.csv", sep=",")
S <- S[,2]

y <- greek$fmv[S]/1000

{t1 <- proc.time()
    set.seed(1)
    numLevels <- c(rep(1, ncol(dat)-2), 2, 19)
    fit <- glinternet.cv(dat[S,], y, numLevels)
    yhat <- predict(fit, dat)
    proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("images/lassohkmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
dev.off()

# qqnorm lot of the residuals 
res_train = y-yhat[S]
qqnorm(res_train);qqline(res_train)
res_test = greek$fmv/1000 - yhat
qqnorm(res_test);qqline(res_test)

# REPLICATING FOR LOG TRANSFORMATION #

shift = abs(min(greek$fmv[S])) + 0.0001

y <- log((greek$fmv[S] + shift)/1000)

{t1 <- proc.time()
    set.seed(1)
    numLevels <- c(rep(1, ncol(dat)-2), 2, 19)
    fit <- glinternet.cv(dat[S,], y, numLevels)
    yhat <- predict(fit, dat)
    proc.time()-t1}
calMeasure(greek$fmv/1000, exp(yhat + as.numeric(var(y-yhat[S]))/2)-(abs(min(greek$fmv[S])) + 0.0001)/1000)

png("images/log_lassohkmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
dev.off()

res_train = y-yhat[S]
qqnorm(res_train);qqline(res_train)

plot(ecdf(res_train));norm
