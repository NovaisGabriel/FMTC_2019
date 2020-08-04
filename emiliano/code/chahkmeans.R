# hierarchical k-means

require(data.tree)
hkmeans <- function(X, k) {
  res <- Node$new("Node 0")
  nCount <- 0
  tmp <- kmeans(X, 2)
  for(i in 1:2) {
    nCount <- nCount + 1
    nodeA <- res$AddChild(paste("Node", nCount))
    nodeA$members <- names(which(tmp$cluster==i))
    nodeA$size <- length(nodeA$members)
    nodeA$center <- tmp$centers[i,]		
  }
	
  while(TRUE) {
    vSize <- res$Get("size", filterFun = isLeaf)
    if(length(vSize) >= k) {
      break
    }
    maxc <- which(vSize == max(vSize))
    nodeL <- FindNode(res, names(maxc))
    tmp <- kmeans(X[nodeL$members,], 2)
    for(i in 1:2) {
      nCount <- nCount + 1
      nodeA <- nodeL$AddChild(paste("Node", nCount))
      nodeA$members <- names(which(tmp$cluster==i))
      nodeA$size <- length(nodeA$members)
      nodeA$center <- tmp$centers[i,]		
    }
  }
  return(res)
}

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

# prepare data
X <- model.matrix( ~ ., data=dat)[,-1]
colnames(X)
summary(X[,1:10])

vMin <- apply(X, 2, min)
vMax <- apply(X, 2, max)
X <- (X - matrix(vMin, nrow=nrow(X), ncol= ncol(X), byrow=TRUE)) / matrix(vMax-vMin, nrow=nrow(X), ncol= ncol(X), byrow=TRUE)
summary(X[,1:10])

{
set.seed(123)
t1 <- proc.time()
res <- hkmeans(X, 340)
proc.time() - t1
}

print(res, "size")
dev.new(width=8, height=6)
par(mar=c(0,4,1,1))
plot(as.dendrogram(res), center = TRUE, leaflab="none")

vSize <- res$Get("size", filterFun = isLeaf)
print(unname(vSize))

par(mar=c(4,4,1,1))
hist(vSize, br=100)

findPolicy <- function(node, X) {
  z <- node$center
  vD <- apply( (X[node$members,] - matrix(z, nrow=node$size, ncol=length(z), byrow=T))^2, 1, sum)
  iMin <- which(vD == min(vD))
  node$policy <- node$members[iMin]
}

res$Do(findPolicy, filterFun = isLeaf, X=X)

vInd <-  res$Get("policy", filterFun = isLeaf) 
vInd <- sort(as.numeric(vInd)) 
print(unname(vInd[1:50]))

summary(datN[vInd,])
table(datC[vInd,])

pairs(datN[vInd,])

write.table(vInd, "hkmeans.csv", sep=",", quote=F, col.names=F)

# generate 680
{
set.seed(123)
t1 <- proc.time()
res <- hkmeans(X, 680)
proc.time() - t1
}

res$Do(findPolicy, filterFun = isLeaf, X=X)

vInd <-  res$Get("policy", filterFun = isLeaf) 
vInd <- sort(as.numeric(vInd)) 
print(unname(vInd[1:50]))

summary(datN[vInd,])
table(datC[vInd,])

write.table(vInd, "hkmeans2.csv", sep=",", quote=F, col.names=F)


