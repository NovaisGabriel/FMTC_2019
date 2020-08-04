library(data.table)
library(MASS)

# Reading the representative VAs IDS
rep_vas = fread('data/raw/hkmeans.csv')
rep_vas_2 = fread('data/raw/hkmeans2.csv')

# reading the whole dataset and merging informations
inforce =fread('data/raw/inforce.csv')
greek= fread('data/raw/Greek.csv')
data = merge(inforce, greek, by = 'recordID')

### First set ofrepresentative VAs ###

# subset of the representative VAs
rep_vas = data[recordID %in% rep_vas$V2]

# QQplot for the fmv, with no transformtions
qqnorm(rep_vas$fmv); qqline(rep_vas$fmv)

# Shifting fmv to positive values only
rep_vas[, fmv_pos := fmv+abs(min(fmv))+0.00001]

# Calculating the best lambda for box-cox transformation, and then transforming the fmv
boxcox = boxcox(fmv_pos~1, data = rep_vas)
lambda = boxcox$x[which(boxcox$y == max(boxcox$y))]
rep_vas[, fmv_boxcox := (fmv_pos^lambda - 1)/lambda]

# plotting the qqplots of the transformed fmvs
qqnorm(rep_vas$fmv_boxcox); qqline(rep_vas$fmv_boxcox)

# plotting the qqplots of the log positive fmvs
qqnorm(log(rep_vas$fmv_pos)); qqline(log(rep_vas$fmv_pos))

### Second set of representative VAs ###

# subset of the representative VAs
rep_vas_2 = data[recordID %in% rep_vas_2$V2]
# QQplot for the fmv, with no transformtions
qqnorm(rep_vas_2$fmv); qqline(rep_vas_2$fmv)
rep_vas_2[, fmv_pos := fmv+abs(min(fmv))+0.0001]
boxcox = boxcox(fmv_pos~1, data = rep_vas_2)
lambda = boxcox$x[which(boxcox$y == max(boxcox$y))]
rep_vas_2[, fmv_boxcox := (fmv_pos^lambda - 1)/lambda]
qqnorm(rep_vas_2$fmv_boxcox); qqline(rep_vas_2$fmv_boxcox)
qqnorm(log(rep_vas_2$fmv_pos)); qqline(log(rep_vas_2$fmv_pos))
