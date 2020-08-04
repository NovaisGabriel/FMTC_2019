library(dplyr)
library(readr)
library(ggplot2)
library(geoR)
library(glinternet)
library(sn)

getwd()
# reset the working directory
setwd("D:/ConferencesWorkshops-2019/FinMathComp-Brazil2019/Code - Sergio Team")
inforce = read.csv("data/raw/inforce.csv");
greek= read.csv("data/raw/Greek.csv");
representative_index = read.csv("data/raw/hkmeans2.csv");

# merging
data = merge(inforce, greek, by = 'recordID');

# creating and treating variables 
data = data %>% mutate(
    age = (currentDate-birthDate)/365,  
    ttm = (matDate - currentDate)/365,
    gender=as.numeric(as.factor(gender))-1, 
    productType=as.numeric(as.factor(productType))-1);

# names of the columns to fit the model

x_columns = c(
    # explanatory variables
    "gbAmt", 
    "gmwbBalance", 
    "withdrawal", 
    paste0("FundValue", c(1:10)), 
    "age", 
    "ttm", 
    "gender", 
    "productType")

columns = c("recordID", x_columns, "fmv")

data = select(data, columns)
train = data %>% filter(recordID %in% representative_index$X75)

numLevels = c(rep(1, length(columns)-4), 2, 19)

head(train)

head(data)

#bx_fit = boxcoxfit(train$fmv, lambda2 = T)
#print(bx_fit)

shift = -min(data$fmv-1000)

#shift

#bx_fit = boxcoxfit(train$fmv + shift)

#bx_fit$lambda

bx_fit = boxcoxfit(train$fmv + shift)
lambda1 = bx_fit$lambda[1]
#lambda2 = bx_fit$lambda[2]
lambda2 = shift
beta = bx_fit$beta
print(bx_fit)
print(c(lambda1, lambda2))

bx_fit$beta

bc.transform <- function(v, l1, l2, beta) {
    y = ((v + l2)^l1 - 1) / l1
    return (y - beta)
}

qqnorm(train$fmv)
qqline(train$fmv)

yy.rep <- bc.transform(train$fmv,lambda1,lambda2, beta)
summary(yy.rep)
hist(yy.rep,br=20)
qqnorm(yy.rep)
qqline(yy.rep)

head(select(train, x_columns))

train$fmv[1:10]

run<-function(data, x_columns, y_column, y_shift=NA, box.cox=FALSE) {
    
    X = select(data, x_columns)
    Y = data[[y_column]]
    
#    if(!is.na(y_shift)) {
#        Y = Y + y_shift
#    }
    
    fit.bc = NA
    lambda1 = NA
    lambda2 = NA
    if(box.cox == TRUE) {
        fit.bc = boxcoxfit(Y+ y_shift)
        lambda1 = fit.bc$lambda[1]
        lambda2 = y_shift
        beta = fit.bc$beta
        Y = bc.transform(Y, lambda1, lambda2, beta)        
    }
    
    fit = glinternet.cv(X=X, Y=Y, numLevels,tol = 1e-5)
    
    residuals = Y - fit$fitted
    
    return(list(
        y_shift = y_shift,
        fit.bc = fit.bc,
        lambda1 = lambda1,
        lambda2 = lambda2,
        beta = beta,
        X = X,
        Y = Y,
        fit = fit,
        residuals = residuals,
        fit.gl = fit
    ))
}

shift

result = run(train, x_columns, "fmv", 0, FALSE)

coef(result$fit.gl)

names(train)

# names(result$fit.gl)
# plot(result$fit.gl)
# plot interactions
plot.interaction <- function(fit,dat){
coeffs <- coef(fit)
# vNames = c(names(dat)[16:17], names(dat)[1:15])
vNames = c(names(dat)[17:18], names(dat)[2:16])
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
    }

plot.interaction(result$fit.gl,train)

result.bc = run(train, x_columns, "fmv", shift, TRUE)

coef(result.bc$fit.gl)

plot.interaction(result.bc$fit.gl,train)

par(mfrow=c(2,2))
qqnorm(train$fmv, main="FMV QQ plot"); qqline(train$fmv)
qqnorm(result$residuals, main="FMV residuals QQ plot"); qqline(result$residuals)
qqnorm(result.bc$Y, main="BC(FMC) QQ plot"); qqline(result.bc$Y)
qqnorm(result.bc$residuals, main="BC(FMC) residuals QQ plot"); qqline(result.bc$residuals)
summary(result$residuals)
summary(result.bc$residuals)
dev.off()

shapiro.test(result$residuals)
shapiro.test(result.bc$residuals)

# representative
fmv.predold = predict(result$fit, train[x_columns])
# library(BBmisc)
plot(train$fmv, fmv.predold,main="representative");abline(0,1)
# full data
fmv.predold = predict(result$fit, data[x_columns])
plot(data$fmv, fmv.predold,pch=".",main="full data");abline(0,1)


# representative
fmv.prednew = predict(result.bc$fit, train[x_columns])
plot(yy.rep, fmv.prednew,main="representative");abline(0,1)
# full data
yy.repn <- bc.transform(data$fmv,lambda1,lambda2, beta)
fmv.prednew = predict(result.bc$fit, data[x_columns])
lim <- range(yy.repn[-33581],fmv.prednew[-33581])
plot(yy.repn[-33581],fmv.prednew[-33581],ylim=lim,xlim=lim,main="full data");abline(0,1)

bc.inverse <- function(v, l1, l2, beta) {
  return (((v+beta) * l1 + 1)^(1/l1) - l2)
}

simulate = function(pred, lambda, sigma, num = 100){
    v = c()
    while (length(v) < num){
        eps = rnorm(num, mean=0, sd=sigma)
        eps = eps[eps > -1/lambda -pred]
        v = c(v, eps)
    }
    return(v[1:num])
}

bc.inverse.mc<-function(data, fit, num=100) {
  sigma = sqrt(var(fit$residuals))
  pred = predict(fit$fit, data)
  values = rep(NA, length(pred))
  for(i in 1:length(values))
  {
    #eps = rnorm(num, mean=0, sd=sigma)
    eps = simulate(pred[i], fit$lambda1, sigma, num)
    values[i] = quantile(bc.inverse(pred[i] + eps, fit$lambda1, fit$lambda2, fit$beta), probs=0.25, na.rm=TRUE)
    #values[i] = mean(bc.inverse(pred[i] + eps, fit$lambda1, fit$lambda2, fit$beta))
  }
  return (values)                       
}

sigma2 = var(result.bc$residuals)
sqrt(sigma2)

hist(bc.inverse(result.bc$residuals,result.bc$lambda1, result.bc$lambda2, result.bc$beta),br=45)
hist(bc.inverse.mc(result.bc$residuals,result.bc$lambda1, result.bc$lambda2, result.bc$beta),br=45)

fmv.pred = predict(result.bc$fit, data[x_columns])

plot(data$fmv, fmv.pred,pch=".");abline(0,1)

values = bc.inverse.mc(train[x_columns], result.bc, num=100)

summary(values)

plot(train$fmv, values);abline(0,1)
plot(train$fmv, 1/values);abline(0,1)

values = bc.inverse.mc(data[x_columns], result.bc, num=1e3)

summary(values)

temp = which(values > 1.5e+6)
print(temp)

plot(data$fmv, values,pch=".",ylim=c(0,1600000));abline(0,1)

temp = order(data$fmv)

head(temp)

representative_index$X2

plot(data$fmv[temp])
abline(h=0.1)
points(representative_index$X2, data$fmv[temp][representative_index$X2], col="red")

plot(data$fmv[temp]);
points(values[temp], col="red")

tail(data[temp,], n=10000)

calMeasure <- function(y, yhat) {
  ae <- sum(yhat - y)
  pe <- sum(yhat - y) / sum(y)
  r2 <- 1 - sum( (yhat-y)^2 )/sum( (y-mean(y))^2 )
  return(list(ae=ae, pe=pe, r2=r2))
}

calMeasure(data$fmv,fmv.predold)
calMeasure(data$fmv[!is.na(values)],values[!is.na(values)])
