#### Testing Box-Cox transformation ####

# Packages #
library(dplyr)
library(readr)
library(rpart)
library(party)
library(randomForest)

# Auxiliary function from emiliano/codes/chaukrig.R
calMeasure = function(y, yhat) {
    pe = sum(yhat - y) / sum(y)
    r2 = 1 - sum( (yhat-y)^2 )/sum( (y-mean(y))^2 )
    mae = sum(abs(y-yhat))/length(y)
    mse = sum((y-yhat)^2)/length(y)
    return(list(pe=pe, r2=r2, mae=mae, mse=mse))
}

# Reading the data:
inforce = read_csv('data/raw/inforce.csv')
greek= read_csv('data/raw/Greek.csv')
representative_index = read_csv('data/raw/hkmeans.csv', col_names = F)

# merging
data = merge(inforce, greek, by = 'recordID')

# creating and treating variables 
data = data %>% mutate(age = (currentDate-birthDate)/365,
                       ttm = (matDate - currentDate)/365,
                       gender= as.factor(gender),
                       productType=as.factor(productType),
                       accountValue = (FundValue1 + FundValue2+ FundValue3+ FundValue4+
                                           FundValue5+ FundValue6+ FundValue7+ FundValue8+
                                           FundValue9+ FundValue10)/1000,
                       gbAmt = gbAmt/1000)

# dividing fmv by 1000
 data = data %>% mutate(fmv = fmv/1000)
 
### First set ofrepresentative VAs ###
 
# subset of the representative VAs
train = data %>% filter(recordID %in% representative_index$X2)

# names of the columns to fit the model
# x_columns = c("gbAmt", "gmwbBalance", "withdrawal", paste0("FundValue", c(1:10)), "age", "ttm",
#               "gender", "productType", "accountValue")

x_columns = c("gbAmt", "gmwbBalance", "withdrawal", "age", "ttm",
              "gender", "productType", "accountValue")

### Conditional tree model ###
fit = ctree(as.formula(paste0('fmv~', paste0(x_columns, collapse = "+"))), data = train, 
            controls = ctree_control(maxdepth = 6, minsplit = 30L))

calMeasure(train$fmv, predict(fit, train))
calMeasure(data$fmv, predict(fit, data))

pred = predict(fit, data)
residuals = data$fmv - pred
plot(residuals)

# fit = rpart(as.formula(paste0('fmv~', paste0(x_columns, collapse = "+"))), data = train, 
#             method = 'anova')
# 
# printcp(fit) # display the results
# plotcp(fit) # visualize cross-validation results
# summary(fit) # detailed summary of splits
# 
# calMeasure(train$fmv, predict(fit, train))
# calMeasure(data$fmv, predict(fit, data))
# 
# # plot tree
# plot(fit, uniform=TRUE,
#      main="Regression Tree for FMV")
# text(fit, use.n=TRUE, all=TRUE, cex=.8)
# 
# fit=  prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# 
# 
# calMeasure(train$fmv, predict(fit, train))
# calMeasure(data$fmv, predict(fit, data))
# 
# fit = ctree(as.formula(paste0('fmv~', paste0(x_columns, collapse = "+"))), data = train)
# 
# calMeasure(train$fmv, predict(fit, train))
# calMeasure(data$fmv, predict(fit, data))
# 
# fit = randomForest(as.formula(paste0('fmv~', paste0(x_columns, collapse = "+"))), data = train)
# calMeasure(train$fmv, predict(fit, train))
# calMeasure(data$fmv, predict(fit, data))
# 
# # com account sem fund value
# 
# fit = ctree(as.formula(paste0('fmv~', paste0(x_columns, collapse = "+"))), data = train, 
#             controls = ctree_control(maxdepth = 6, minsplit = 30L))
