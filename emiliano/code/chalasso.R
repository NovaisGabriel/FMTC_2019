# linear models with interactions

require(glinternet)

# load data
setwd("/home/novais/Desktop/Projetos/FMTC2019/emiliano/code")
setwd("/home/novais/Desktop/Projetos/FMTC2019/emiliano/code")
inforce <- read.csv("inforce.csv")

vNames <- c("gbAmt", "gmwbBalance", "withdrawal",  paste("FundValue", 1:10, sep=""))

age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)

datN <- cbind(inforce[,vNames], data.frame(age=age, ttm=ttm))
datC <- inforce[,c("gender", "productType")]

dat <- cbind(datN, gender=as.numeric(datC$gender)-1, 
  productType=as.numeric(datC$productType)-1)
summary(dat)

greek <- read.csv("Greek.csv")
greek <- greek[order(greek$recordID),]


# based on lhs
S <- read.table("lhs.csv", sep=",")
S <- S[,2]

y <- greek$fmv[S]/1000

{t1 <- proc.time()
set.seed(1)
numLevels <- c(rep(1, ncol(dat)-2), 2, 19)
fit <- glinternet.cv(dat[S,], y, numLevels)
yhat <- predict(fit, dat)
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("lassolhs.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
dev.off()

# based on clhs
S <- read.table("clhs.csv", sep=",")
S <- S[,2]

y <- greek$fmv[S]/1000

{t1 <- proc.time()
set.seed(1)
numLevels <- c(rep(1, ncol(dat)-2), 2, 19)
fit <- glinternet.cv(dat[S,], y, numLevels)
yhat <- predict(fit, dat)
proc.time()-t1}
calMeasure <- function(y, yhat) {
    pe <- sum(yhat - y) / sum(y)
    r2 <- 1 - sum( (yhat-y)^2 )/sum( (y-mean(y))^2 )
    return(list(pe=pe, r2=r2))
}
calMeasure(greek$fmv/1000, yhat)

png("lassoclhs.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
dev.off()

# based on hkmeans
S <- read.table("hkmeans.csv", sep=",")
S <- S[,2]

y <- greek$fmv[S]/1000

{t1 <- proc.time()
set.seed(1)
numLevels <- c(rep(1, ncol(dat)-2), 2, 19)
fit <- glinternet.cv(dat[S,], y, numLevels)
yhat <- predict(fit, dat)
proc.time()-t1}
calMeasure(greek$fmv/1000, yhat)

png("lassohkmeans.png", width=8, height=4, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
qqplot(greek$fmv/1000, yhat, xlab="FMV(MC)", ylab="FMV(LM)")
abline(0,1)
dev.off()

# plot interactions
coeffs <- coef(fit)
vNames = c(names(dat)[16:17], names(dat)[1:15])
par(mar=c(7,7,1,1))
plot(0,type="n",xlim=c(0, 17), ylim=c(0, 17), axes=FALSE,ann=FALSE)
grid(17,17, col="black")
vLim <- par("usr")
dLen <- (vLim[2] - vLim[1]) / 17
box()
axis(1, at=seq(vLim[1] + 0.5*dLen, vLim[2], dLen), labels=vNames, las=2)
axis(2, at=seq(vLim[1] + 0.5*dLen, vLim[2], dLen), labels=vNames, las=2)

mInter <- coeffs$interactions$catcat
for(i in 1:nrow(mInter)) {
  points((mInter[i,1]-0.5)*dLen+vLim[1], (mInter[i,2]-0.5)*dLen+vLim[1], pch=19)
  points((mInter[i,2]-0.5)*dLen+vLim[1], (mInter[i,1]-0.5)*dLen+vLim[1], pch=19)
}
mInter <- coeffs$interactions$contcont
for(i in 1:nrow(mInter)) {
  points((mInter[i,1]+1.5)*dLen+vLim[1], (mInter[i,2]+1.5)*dLen+vLim[1], pch=19)
  points((mInter[i,2]+1.5)*dLen+vLim[1], (mInter[i,1]+1.5)*dLen+vLim[1], pch=19)
}
mInter <- coeffs$interactions$catcont
for(i in 1:nrow(mInter)) {
  points((mInter[i,1]-0.5)*dLen+vLim[1], (mInter[i,2]+1.5)*dLen+vLim[1], pch=19)
  points((mInter[i,2]+1.5)*dLen+vLim[1], (mInter[i,1]-0.5)*dLen+vLim[1], pch=19)
}

