# installing packages
install.packages("glinternet")
install.packages("dplyr")

# importing packages
library("glinternet")
library("dplyr")

# getting directory
getwd()

# reset the working directory
setwd("/home/novais/Desktop/Projetos/FMTC2019/data/raw")

# importing data
inforce <- read.csv("inforce.csv",header=T)
greek <- read.csv("Greek.csv",header=T)

# define two new variables in inforce
age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)
inforce <- cbind(inforce, data.frame(age=age, ttm=ttm))

# merge data frames
inforce2 <- merge(inforce, greek, '/home/novais/Desktop/Projetos/FMTC2019/data/raw', by.x="recordID", by.y="recordID")

vNames <- c("recordID", "gender", "productType",
            "age", "ttm", "gbAmt", "gmwbBalance",
            "wbWithdrawalRate",'withdrawal', paste("FundValue", 1:10, sep=""), "fmv")
inforce2 <- inforce2[,vNames]
head(inforce2)
names(inforce2)

write.csv(inforce2,"/home/novais/Desktop/Projetos/FMTC2019/codes/NN/inforce2.csv")
# samples
sample1id <-read.csv("hkmeans.csv",header=FALSE)
sample2id <-read.csv("hkmeans2.csv",header=FALSE)
names(sample1id) <- c("index", "recordID")
names(sample2id) <- c("index", "recordID")

sample1=inforce2[inforce2$recordID %in% sample1id$recordID, ]
sample2=inforce2[inforce2$recordID %in% sample2id$recordID, ]

write.csv(sample1,"/home/novais/Desktop/Projetos/FMTC2019/codes/NN/sample1.csv")

#LASSO
Y <- sample1$fmv
X <- subset( sample1, select = -c(fmv,recordID) )
X$productType = as.numeric(as.factor(X$productType))-1
X$gender = as.numeric(as.factor(X$gender))-1
numLevels = c(2,19,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
fit = glinternet(X, Y, numLevels)
coeffs = coef(fit)

fit_cv = glinternet.cv(X,Y,numLevels)
pred = predict(fit_cv, X)
res = Y - pred
shapiro.test(res)
qqnorm(res);qqline(res)
