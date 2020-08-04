#### Testing Box-Cox transformation ####

# Packages #
library(dplyr)
library(readr)
library(ggplot2)
library(zoo)

# Reading the data:
inforce = read_csv('data/raw/inforce.csv')
greek= read_csv('data/raw/Greek.csv')
representative_index = read_csv('data/raw/hkmeans.csv', col_names = F)

# merging
data = merge(inforce, greek, by = 'recordID')

# creating and treating variables 
data = data %>% mutate(age = ((currentDate-birthDate)/365)/70,  
                       ttm = ((matDate - currentDate)/365)/30,
                       gender=as.numeric(as.factor(gender)), 
                       productType=as.numeric(as.factor(productType)),
                       accountValue = (FundValue1 + FundValue2+ FundValue3+ FundValue4+
                                          FundValue5+ FundValue6+ FundValue7+ FundValue8+
                                          FundValue9+ FundValue10)/100000,
                       gbAmt = gbAmt/1000000,
                       gmwbBalance = gmwbBalance/1000000)

data = data %>% mutate(productType = ifelse(productType %in% c("DBRP", "DBRU", "DBSU"), 
                                            "death",
                                            ifelse(productType %in% c("ABRP", "ABRU", "ABSU",
                                                                      "IBRP", "IBRU", "IBSU",
                                                                      "MBRP", "MBRU", "MBSU",
                                                                      "WBRP", "WBRU", "WBSU"),
                                                   "living", "both")))
data = data %>% mutate(productType = as.numeric(as.factor(productType)))

# dividing fmv by 1000
data = data %>% mutate(fmv = fmv/1000)

### First set ofrepresentative VAs ###

# subset of the representative VAs
x_columns = c("gender", "productType", "gbAmt", "gmwbBalance", "withdrawal",
              "accountValue", "age", "ttm")
rep = data %>% filter(recordID %in% representative_index$X2) 
rep_fmv = rep %>% select(recordID, fmv)
rep = rep %>% select(x_columns)

# subset of other contracts
data = data %>% filter(!recordID %in% representative_index$X2) 
data_fmv = data %>% select(recordID, fmv)
data = data %>% select(c('recordID', x_columns))

order_vec = c()
for (i in 1:14){
    order_vec = c(order_vec, seq(from = i, by = 340, length.out = 14))
}
dif_vas = function(va){
    id = va['recordID']
    va = va[-1]
    va_df = do.call(rbind, replicate(nrow(rep), coredata(va), simplify = FALSE))
    dif = va_df - rep
    dif = dif %>% mutate(gender_f = ifelse(gender== 0, 1, 0))
    dif = dif %>% mutate(productType_f = ifelse(productType== 0, 1, 0))
    dif = dif %>% mutate_at(x_columns[-c(1,2)], .funs = list(funpos = ~ . + 0))
    dif = dif %>% mutate_at(x_columns[-c(1,2)], .funs = list(funneg = ~ . + 0))
    dif = dif %>% mutate_at(vars(contains('funpos')), .funs = list(~ ifelse(. > 0, ., 0)))
    dif = dif %>% mutate_at(vars(contains('funneg')), .funs = list(~ ifelse(. < 0, ., 0)))
    dif = dif %>% select(-x_columns)
    dif = unlist(dif)[order_vec]
    return(c('recordID', dif))
}

#saving rep contracts
write_csv(rep, 'data/treated/rep_x.csv')
write_csv(rep_fmv, 'data/treated/rep_y.csv')

# function to generate and save data bases
gen_data = function(lines, name){
    train = data.frame(t(apply(data[lines, ], 1, dif_vas)))
    write_csv(train, paste0('data/treated/dist_', name, '.csv'))
}

set.seed(0)
train_index = sample(c(1:nrow(data)), size = 500)
test_index = c(1:nrow(data))[-train_index]

gen_data(train_index, 'train')

i = 0
for (j in c(1:189)){
    if (j<189){
        gen_data(test_index[c(i+1:i+1000)], j)
    }else{
        gen_data(test_index[c(i+1:nrow(data))], j)
    }
    i = i + 1000
}



