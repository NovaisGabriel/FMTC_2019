# Loading datasets
fmv = read.csv("/home/novais/Desktop/Projetos/FMTC2019/principal/vamc/datasets/fmv_seriatim.csv",sep=',',header = TRUE)
inforce = read.csv("/home/novais/Desktop/Projetos/FMTC2019/principal/vamc/datasets/inforce.csv",sep=',',header = TRUE)
greek = read.csv("/home/novais/Desktop/Projetos/FMTC2019/principal/vamc/datasets/Greek.csv", sep = ',', header = T)

# Note that fmv[,'base.base']) id the same of greek[, 'fmv']

# Defining producType as  factor 
type = as.factor(inforce$productType)
unique(type)

# Some plots
plot(greek$fmv[which()], col = type)
# data = merge(greek[, c('recordID', 'fmv')], inforce[, c('recordID', 'productType')], by = 'recordID')
data = merge(greek, inforce, by = 'recordID')
ABRP = data[data$productType == 'ABRP', ]
plot(ABRP$birthDate, ABRP$fmv)

plot(type, greek$fmv)

plot(data[, -c('currentDate', 'FundValue1')])
plot(age, greek$fmv)


age = inforce$currentDate - inforce$birthDate
teste = ABRP[, c(fmv, "issueDate")]

library('stats')
x = data[,c('proType','fmv')]
model = kmeans(x = x, center = 19, iter.max = 300, algorith = 'Lloyd')
