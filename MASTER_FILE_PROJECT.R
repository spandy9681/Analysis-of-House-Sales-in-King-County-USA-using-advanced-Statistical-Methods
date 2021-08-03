# Enire Data Analysis Project All IN ONE
#Fix seed value to mimic outcomes
set.seed(69)

# Required Libraries
library(readr)
library(splines)
library(glmnet)
library(boot)
library(leaps)
library(tidyverse)
library(caret)
library(ggplot2)
library(ggpubr)
library(magrittr)

#Importing the data and properly naming things
Data_org = kc_house_data_original
Data_House = kc_house_data_to_be_used
X = kc_house_data_original

# Knowing the names of the variables
vec_names = names(Data_House)
vec_names

#checking missing observations
sum(is.na(Data_House))

# overview through summary function
data_summary = summary(Data_House)
data_summary
str(Data_House)

# dimension of the dataset
dim(Data_House)

#attaching the dataset
attach(Data_House)

#histogram and barplots
t_bathrooms = table(bathrooms)
barplot(t_bathrooms)
t_bedrooms = table(bedrooms)
barplot(t_bedrooms)
t_floors = table(floors)
barplot(t_floors)
t_waterfront = table(waterfront)
barplot(t_waterfront)
t_view = table(view)
barplot(t_view)
t_condition = table(condition)
barplot(t_condition)
t_grade = table(grade)
barplot(t_grade)

#histograms
hist(price)
hist(sqft_living)
hist(sqft_lot)

#boxplots for getting idea about effects of different factors
boxplot(price~bedrooms)
boxplot(price~bathrooms)
boxplot(price~floors)
boxplot(price~waterfront)
boxplot(price~view)
boxplot(price~condition)
boxplot(price~grade)

#some further exploratory data analysis
ind_renov = rep(0,nrow(Data_House))
length(price)
ind_renov[yr_renovated != 0] <- 1
boxplot(price~ind_renov)
hist(yr_renovated[ind_renov == 1])

# Showing interaction b/w bathroom and bedrooms
data_subset = Data_House[,c(1,2,3)]
data_subset$bathrooms = round(data_subset$bathrooms)
head(data_subset)
data_subset$bedrooms = as.factor(data_subset$bedrooms)
data_subset$bathrooms = as.factor(data_subset$bathrooms)
ggboxplot(data_subset, x = "bedrooms", y = "price", color = "bathrooms")

#Some plots for continuous valued variables
hist(price)
hist(sqft_living)
plot(sqft_living,price)
detach(Data_House)

# Confirmatory Data Analysis

# Selecting a subset of the original data
c=sample(1:nrow(X),250,replace=F)
Y=X[c,] #---The sample observations
which(Y$price == sort(Y$sqft,decreasing = T)[2])
Y[233,] = Y[235,]
attach(Y)
renovation_indicator=as.integer(yr_renovated>0)
range(sqft_basement)
range(price)
boxplot(price)
#It indicates the house is renovated or not
#---Check the effect of quatitive variables---

#-----Relationship with the area of  basement---
lr=lm(price~sqft_basement)
summary(lr)

#(comment)----significant effect--
#---kernal regression--
plot(sqft_basement,price,xlab="Area of basement",main="Fitting kernel regression")#Scatter plot
#Gaussian Kernel
lines(ksmooth(sqft_basement,price,kernel="normal",bandwidth=300,n.points=length(sqft_living),range(sqft_basement)),col="blue")##---Fitting Normal Kernel regression
#Box Kernel
lines(ksmooth(sqft_basement,price,kernel="box",
              bandwidth=300,n.points=length(sqft_living),range(sqft_basement)),col="red")##----Fitting KNN or Box kernel 
legend("topright",legend=c("Normal","Box"),lty=1:1,col=c("blue","red"))

#Scatter plot
plot(sqft_basement,price,xlab="Area of basement",main="Fitting smoothing spline")
lines(smooth.spline(sqft_basement,price))#---fitting smoothing spline---
#----choose the degree the of polynomial by comparing measure of optimism viz AIC,BIC or CV---
cv.error.10 = numeric(10);a=numeric(10);b=numeric(10)
for(i in 1:5)
{
  a[i]=AIC(lm(price~poly(sqft_basement,i,raw=T)))
  b[i]=BIC(lm(price~poly(sqft_basement,i,raw=T)))
  glm.fit=glm(price~poly(sqft_basement,i,raw=T) ,data=Y)
  cv.error.10[i] = cv.glm(Y ,glm.fit,K = 10)$delta[1]
}
plot(cv.error.10,type="l")#----plotting the AIC to have it's minimum value
plot(a,type="l")#----plotting the AIC to have it's minimum value
plot(b,type="l")#----plotting the BIC to have it's minimum value
#----Polynomial of degree 1  or simple regression is appropriate--- 
x=sqft_basement
y=price
fit <- lm(y ~ x)   
plot(sqft_basement,price,xlab="Area of basement",main="Fitting polynomial regression")#scatter plot
abline(fit) ## fit regression curve 
#------Relationship with the area of above---
lr=lm(price~sqft_above)
summary(lr)
#(comment)----significant effect--

#---kernal regression--
plot(sqft_above,price,xlab="Area of above",main="Fitting Kernel regression")
lines(ksmooth(sqft_above,price,kernel="normal"
              ,bandwidth=300,n.points=length(sqft_above),range(sqft_above)),col="blue")##---Fitting Normal Kernel regression
lines(ksmooth(sqft_above,price,kernel="box",bandwidth=300,
              n.points=length(sqft_above),range(sqft_above)),col="red")##---Fitting KNN or Box Kernel regression
legend("topright",legend=c("Normal","Box"),lty=1:1,col=c("blue","red"))
plot(sqft_basement,price,xlab="Area of above",main="Fitting smoothing spline")
lines(smooth.spline(sqft_basement,price))#---fitting smoothing spline---
#----choose the degree the of polynomial by comparing measure of optimism viz AIC,BIC or CV---
a=numeric(10);b=numeric(10)
for(i in 1:10)
{
  a[i]=AIC(lm(price~poly(sqft_above,i,raw=T)))
  b[i]=BIC(lm(price~poly(sqft_above,i,raw=T)))
}
plot(a,type="l")#----plotting the AIC to have it's minimum value
plot(b,type="l")#----plotting the BIC to have it's minimum value
x=sqft_above
y=price
fit <- lm(y ~x)
plot(sqft_basement,price,xlab="Area of above",main="Fitting polynomial regression")#scatter plot
abline(fit)#Fit simple regression
#----ANOVA model to the relationship with qualitative varables---
summary(aov(price~as.factor(bedrooms)))#--significant effect
summary(aov(price~as.factor(bathrooms)))#--significant effect
summary(aov(price~as.factor(floors)))#--significant effect
summary(aov(price~as.factor(waterfront)))#--significant effect
summary(aov(price~as.factor(view)))#--significant effect
summary(aov(price~as.factor(condition)))#--No significant effect
summary(aov(price~as.factor(grade)))#--significant effect
summary(aov(price~as.factor(renovation_indicator)))#--No significant effect
summary(aov(price~as.factor(bedroom_per_bathroom)))#--significant effect

# cross validation 
ind = sample(nrow(Data_House),size = 500)
head(ind)
data_subset = Data_House[ind,]
cv.error.5 = rep(0,5)
for(i in 1:5)
{
  glm.fit=glm(price~poly(sqft_living,i) ,data=data_subset)
  cv.error.5[i] = cv.glm(data_subset ,glm.fit,K = 10)$delta[1]
}
cv.error.5
plot(cv.error.5,type = "l")
which(cv.error.5 == min(cv.error.5)) 

# Best subset selection

names(data_subset)
regfit.full = regsubsets(price~.,data_subset[,-4])
regfit.full$
  reg_summary = summary(regfit.full)
names(reg_summary)
reg_summary$rsq
reg_summary$rss
reg_summary$adjr2
reg_summary$cp
reg_summary$bic

plot(reg_summary$rsq,type = "l")
plot(reg_summary$rss,type = "l")
plot(reg_summary$adjr2,type = "l")
plot(reg_summary$cp,type = "l")
plot(reg_summary$bic,type = "l")

# Ridge and Lasso

x=model.matrix(price~.,data_subset)[,-1]
y=data_subset$price
grid=10^seq(10,-2, length =10)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda
coef(ridge.mod)[,6]
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test ,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=10^6 ,newx=x[test ,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0,lambda = bestlam)
predict(out,type="coefficients",s=bestlam)

#LASSO

lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(x[train ,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test ,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]

#Splines

#Basic Spline
sqft_lims=range(data_subset$sqft_living)
sqft.grid=seq(from=sqft_lims [1],to=sqft_lims [2])
fit=lm(price~bs(sqft_living ,df = 15),data=data_subset)
pred=predict(fit,newdata=list(sqft_living=sqft.grid),se=T)
plot(data_subset$sqft_living,data_subset$price,col="gray")
lines(sqft.grid,pred$fit ,lwd=2)
lines(sqft.grid,pred$fit +2*pred$se ,lty="dashed")
lines(sqft.grid,pred$fit -2*pred$se ,lty="dashed")

#Natural Spline

fit2=lm(price~ns(sqft_living,df=4),data=data_subset)
pred2=predict(fit2 ,newdata=list(sqft_living=sqft.grid),se=T)
lines(sqft.grid, pred2$fit ,col="red",lwd=2)

#Smoothing Spline

plot(data_subset$sqft_living,data_subset$price,xlim=sqft_lims ,cex=.5,col="darkgrey ")
title("Smoothing Spline")
fit=smooth.spline(data_subset$sqft_living,data_subset$price,df=16)
fit2=smooth.spline(data_subset$sqft_living,data_subset$price,cv=TRUE)
fit2$df
lines(fit ,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),
       col=c("red","blue"),lty=1,lwd=2,cex=.8)
