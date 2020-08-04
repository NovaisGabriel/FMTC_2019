# conditional Latin hypercube sampling

require(clhs)

set.seed(1)
df <- data.frame(a = runif(1000), b = rnorm(1000))

set.seed(1)
res <- clhs(df, size = 50, iter = 100, simple = T)
res

dev.new(width=8,height=4)
par(mfrow=c(1,2),mar=c(4,4,1,1))
plot(df$a,df$b, xlim=range(df$a), ylim=range(df$b))
plot(df$a[res],df$b[res], xlim=range(df$a), ylim=range(df$b))

#########
# Select representative policies
#########

# Load data
setwd("C:/Users/Guojun Gan/Downloads/vamc/datasets")
setwd("/Volumes/Scratch/gan/MathDepartment/Data/vamc/datasets")
inforce <- read.csv("inforce.csv")

vNames <- c("gbAmt", "gmwbBalance", "withdrawal",  paste("FundValue", 1:10, sep=""))

age <- with(inforce, (currentDate-birthDate)/365)
ttm <- with(inforce, (matDate - currentDate)/365)

datN <- cbind(inforce[,vNames], data.frame(age=age, ttm=ttm))
datC <- inforce[,c("gender", "productType")]
dat <- cbind(datN, datC)
head(dat)

summary(dat)

set.seed(1)
{
t1 <- proc.time()
res <- clhs(dat, size = 340, iter = 1000, simple = T)
proc.time() - t1
}
res <- sort(res)
res[1:50]

summary(datN[res,])
table(datC[res,])

pairs(datN[res,])

write.table(res, "clhs.csv", sep=",", quote=F, col.names=F)

# generate 680
set.seed(1)
{
t1 <- proc.time()
res <- clhs(dat, size = 680, iter = 1000, simple = T)
proc.time() - t1
}
res <- sort(res)
res[1:50]
write.table(res, "clhs2.csv", sep=",", quote=F, col.names=F)

