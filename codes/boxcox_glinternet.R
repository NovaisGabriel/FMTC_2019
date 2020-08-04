#### Testing Box-Cox transformation ####

# Packages #
library(dplyr)
library(readr)
library(ggplot2)
library(geoR)
library(glinternet)

# Auxiliary function from emiliano/codes/chaukrig.R
calMeasure = function(y, yhat) {
    pe = sum(yhat - y) / sum(y)
    r2 = 1 - sum( (yhat-y)^2 )/sum( (y-mean(y))^2 )
    return(list(pe=pe, r2=r2))
}

# Auxiliary funtion to build ppplot 
ppplot = function(x){
    set.seed(1)
    N = length(x)
    x = sort(x)
    n.props = pnorm(x, mean(x), sd(x))
    props = 1:N / (N+1)
    ggplot(data = data.frame(props, n.props), aes(x=props, y= n.props)) + geom_point() + geom_abline()
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
                       gender=as.numeric(as.factor(gender))-1, 
                       productType=as.numeric(as.factor(productType))-1)
# dividing fmv by 1000
data = data %>% mutate(fmv = fmv/1000)

### First set ofrepresentative VAs ###

# subset of the representative VAs
train = data %>% filter(recordID %in% representative_index$X2)

# names of the columns to fit the model
x_columns = c("gbAmt", "gmwbBalance", "withdrawal", paste0("FundValue", c(1:10)), "age", "ttm",
              "gender", "productType")

#levels of each column
numLevels = c(rep(1, length(x_columns)-2), 2, 19)

# list of qqplots to be filled:
qqplots_train = list()
qqplots_all = list()
# list of ppplots to be filled:
ppplots_train = list()
ppplots_all = list()

# function: train a glinternet model and returns predicted values
glinternet_fit_predict = function(train_data, whole_data, x_columns, numLevels, y_column){
    t1 = proc.time()
    set.seed(1)
    fit = glinternet.cv(X = select(train_data, x_columns), Y = train_data[[y_column]], numLevels)
    yhat_train = fit$fitted
    yhat_all = as.numeric(predict(fit, select(whole_data, x_columns)))
    print(proc.time()-t1)
    return(list("yhat_train" = yhat_train, "yhat_all" = yhat_all))
}

# First model: no transformation
yhats = glinternet_fit_predict(train, data, x_columns, numLevels, "fmv")
# adding the predictions as columns
train = train %>% mutate(yhat_no_t = yhats$yhat_train)
data = data %>% mutate(yhat_no_t = yhats$yhat_all)
# Measure: training
calMeasure(train$fmv, train$yhat_no_t)
# Measure: whole dataset
calMeasure(data$fmv, data$yhat_no_t)
# updating list of plots
qqplots_train[['no_transformation']] = ggplot(train, aes(sample = (fmv - yhat_no_t))) +
    stat_qq() + stat_qq_line()
ppplots_train[['no_transformation']] = ppplot(train$fmv - train$yhat_no_t)

# qqplots_all[['no_transformation']] = ggplot(data, aes(sample = (fmv - yhat_no_t))) +
#     stat_qq() + stat_qq_line()

# Box-Cox transformation
# fitting the boxcox
shift = 100
bx_fit = boxcoxfit(train$fmv + shift)
lambda2 = shift
bx_fit = boxcoxfit(train$fmv, lambda2 = T)
lambda1 = bx_fit$lambda[1]
lambda2 = bx_fit$lambda[2]
#data = data %>% mutate(fmv_bx = ((fmv+lambda2)^lambda1 - 1)/lambda1)
train = train %>% mutate(fmv_bx = ((fmv+lambda2)^lambda1 - 1)/lambda1)

# Second model: boxcox transformation
yhats = glinternet_fit_predict(train, data, x_columns, numLevels, "fmv_bx")
# adding the predictions as columns
train = train %>% mutate(yhat_bx = yhats$yhat_train)
data = data %>% mutate(yhat_bx = yhats$yhat_all)
# transforming the predictions back
train = train %>% mutate(yhat_bx_back = ((yhat_bx*lambda1 + 1)^(1/lambda1)) - lambda2)
data = data %>% mutate(yhat_bx_back = ((yhat_bx*lambda1 + 1)^(1/lambda1)) - lambda2)
# Measure: training: transformed
calMeasure(train$fmv_bx, train$yhat_bx)
# Measure: training: back
calMeasure(train$fmv, train$yhat_bx_back)
# Measure: whole dataset: transformed back
calMeasure(data$fmv, data$yhat_bx_back)
# updating list of plots
qqplots_train[['boxcox']] = ggplot(train, aes(sample = (fmv_bx - yhat_bx))) +
    stat_qq() + stat_qq_line()
ppplots_train[['boxcox']] = ppplot(train$fmv_bx - train$yhat_bx)

# qqplots_all[['boxcox']] = ggplot(data, aes(sample = (bx_fmv - yhat_bx))) +
#     stat_qq() + stat_qq_line()


# Log transformation
shift = abs(min(data$fmv)) + 0.000001
train = train %>% mutate(fmv_log = log(fmv + shift))
data = data %>% mutate(fmv_log = log(fmv + shift))
# Third model: log transformation
yhats = glinternet_fit_predict(train, data, x_columns, numLevels, "fmv_log")
# adding the predictions as columns
train = train %>% mutate(yhat_log = yhats$yhat_train)
data = data %>% mutate(yhat_log = yhats$yhat_all)
# transforming the predictions back
train = train %>% mutate(yhat_log_back = exp(yhat_log + var(yhat_log)/2) - shift)
data = data %>% mutate(yhat_log_back = exp(yhat_log + var(yhat_log)/2) - shift)
# Measure: training: transformed
calMeasure(train$fmv_log, train$yhat_log)
# Measure: training: transformed back
calMeasure(train$fmv, train$yhat_log_back)
# Measure: whole dataset
calMeasure(data$fmv, data$yhat_log_back)
# updating list of plots
qqplots_train[['log']] = ggplot(train, aes(sample = (fmv_log - yhat_log))) +
    stat_qq() + stat_qq_line()
ppplots_train[['log']] = ppplot(train$fmv_log - train$yhat_log)

### PLOTS ###
# No Trnasformation
qqplots_train$no_transformation
ppplots_train$no_transformation

# Boxcox
qqplots_train$boxcox
ppplots_train$boxcox

# Log
qqplots_train$log
ppplots_train$log
