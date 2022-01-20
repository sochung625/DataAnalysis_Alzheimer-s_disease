##AMS 572 group 12
# Dennis Feng, Hongbo Yin, Soyoung Chung and Zhiyi Da

install.packages(c("mice","VIM"))
install.packages("leaps")
install.packages("Rcpp")
install.packages("lattice")
install.packages("parallel")

library(mice)
library(VIM)
library(leaps)
library(simFrame)
library(Rcpp)
library(lattice)

## Methodology
setwd("C:/Users/86188/Downloads")
data = read.csv('oasis_cross-sectional.csv')

data$M.F=as.factor(data$M.F)
#Female=1; Male =2;
str(data$M.F)
mymatrix=matrix(c(data$M.F,data$nWBV),ncol = 2,byrow = F)

nWBV_F=mymatrix[which(mymatrix[,1]==1),]
nWBV_M=mymatrix[which(mymatrix[,1]==2),]

var.test(nWBV_F[,2],nWBV_M[,2])

res<-t.test(nWBV_F[,2],nWBV_M[,2], var.equal = TRUE)
res
res$p.value

## Missing Values at Random(MCAR)
mydata <- read.csv("oasis_cross-sectional.csv")
mydataFrame<-as.data.frame(mydata)
nac<-NAControl(NArate=0.2)
x<-setNA(mydataFrame,nac)
M.F_MAR=as.factor(x$M.F)
mymatrix_MAR=matrix(c(M.F_MAR,x$nWBV),ncol = 2,byrow = F)

### 1. Delete missing values
newdata=na.omit(mymatrix_MAR)
nWBV_F_MAR=newdata[which(newdata[,1]==1),]
nWBV_M_MAR=newdata[which(newdata[,1]==2),]

### 2. Check equal variance assumption
var.test(nWBV_F_MAR[,2],nWBV_M_MAR[,2])

### 3. Two sample t-test with equal variance
t.test(nWBV_F_MAR[,2],nWBV_M_MAR[,2],var.equal = TRUE)

## Effect of missing data
### 1. Missing Values Not At Random(MNAR): delete missing values
nWBV_F_MNAR=replace(nWBV_F[,2],1:100,NA)

# Replace first 100 values in nWBV of female to be missing values
new_nWBV_F_MNAR=na.omit(nWBV_F_MNAR)
# Check equal variance assumption**

var.test(new_nWBV_F_MNAR,nWBV_M[,2])

### 3. Two sample t-test with equal variance
t.test(new_nWBV_F_MNAR,nWBV_M_MAR[,2],var.equal = TRUE)


#########################################
#### Multiple Regression Analysis #######
#########################################

setwd("C:/Users/86188/Downloads")
data = read.csv('oasis_cross-sectional.csv',stringsAsFactors = TRUE)
# Verify that the data is converted to the form we expected earlier
str(data)

### 1. Analyze whether missing value exists in data.
# sum of the rows with one or more missing values
sum(!complete.cases(data))
mean(is.na(data)) #15.7%of instances have missing values
mean(!complete.cases(data))#50% of the instances contained one or more missing values


md.pattern(data)

#aggr () function generates a pattern of missing values for the dataset
aggr(data,prop=F,numbers=T)
matrixplot(data)

### 2. To use Multiple interpolation to impute data.
knitr::include_graphics("C:/Users/86188/Downloads/mice.png")

imp = mice(data,seed = 1234)

ok = with(imp,lm(nWBV~M.F+Age+Educ+SES+MMSE+CDR+eTIV+ASF,data=data))
pooled = pool(ok)
summary(pooled)
data2 = complete(imp,action = 3)
stripplot(imp,pch=19,cex=1.2,alpha=.3)

### 3. Building model
fit = lm(nWBV~M.F+Age+Educ+SES+MMSE+CDR+eTIV+ASF,data = data2)
summary(fit)

### 4. Use Best subsets regression to find the best model.
leap = regsubsets(nWBV~M.F+Age+Educ+SES+MMSE+CDR+eTIV+ASF,data = data2)
summary(leap)
leap_s = summary(leap)
names(leap_s)
leap_s$rsq

data.frame(
  adj_r2_max = which.max(leap_s$adjr2),
  cp_min = which.min(leap_s$cp),
  bic_min = which.min(leap_s$bic)
)

par(mfrow = c(1,2))
plot(leap, scale = "r2")
plot(leap, scale = "adjr2")
par(mfrow = c(1,2))
plot(leap, scale = "Cp")
plot(leap, scale = "bic")

coef(leap, 4)

f = lm(nWBV~Age+MMSE+CDR+eTIV,data=data2)
summary(f)

### 5. Effect of missing data.
fit1=lm(nWBV~M.F+Age+Educ+SES+MMSE+CDR+eTIV+ASF,data=data)
summary(fit1)

fit2 = lm(nWBV~M.F+Age+Educ+SES+MMSE+CDR+eTIV+ASF,data = data2)
summary(fit2)

par(mfrow = c(2,2))
plot(fit1)

par(mfrow = c(2,2))
plot(fit2)

adjusteddata<-data[sample(nrow(data),0.8*nrow(data)),]
nrow(adjusteddata)
adjustedM.F<-adjusteddata$M.F
adjustednWBV<-adjusteddata$nWBV
adjustedAge<-adjusteddata$Age
adjustedEduc<-adjusteddata$Educ
adjustedSES<-adjusteddata$SES
adjustedMMSE<-adjusteddata$MMSE
adjustedCDR<-adjusteddata$CDR
adjustedeTIV<-adjusteddata$eTIV
adjustedASF<-adjusteddata$ASF

lm(nWBV~M.F+Age+Educ+SES+MMSE+CDR+eTIV+ASF,data = data2)

fit3<-lm(adjustednWBV~adjustedM.F+adjustedAge+adjustedEduc+adjustedSES+adjustedMMSE+adjustedCDR+adjustedeTIV+adjustedASF,data=adjusteddata)
summary(fit3)
par(mfrow = c(2,2))
plot(fit3)
