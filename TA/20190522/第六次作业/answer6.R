#Q1
rm(list = ls())
a<-read.table("/Users/qiguangyao/Public/Courses/bioSta/20190522/第六次作业/homework-6.1-data.csv",header = T, sep = ",")
ff<-lm(a[,3]~a[,1]+a[,2])
summary(ff)
#Q2
rm(list = ls())
a <- read.table('/Users/qiguangyao/Public/Courses/bioSta/20190522/第六次作业/homework-6.2-data.csv',sep = ',',header = TRUE)
fit<-lm(y~x1+x2,data=a)
summary(fit)
confint(fit,level=0.95)
#Q3
rm(list = ls())
wine_data <- read.csv("/Users/qiguangyao/Public/Courses/bioSta/20190522/第六次作业/homework-6.3-winequality-red.csv", sep=",",header=T)
#1)
head(wine_data,5)
summary(wine_data)
#2)
hist(wine_data$fixed.acidity,main="the distribution of fixed acidity",xlab="volatile.acidity")
plot(wine_data$quality,wine_data$volatile.acidity,main="quality vs volatile acidity",xlab="quality",ylab="volatile.acidity",col=6,pch=8)
#3)
apply(wine_data,2,function(x)cor(x,wine_data$quality))
#4)
#
shapiro.test(wine_data$alcohol)
shapiro.test(wine_data$quality)
bartlett.test(wine_data$alcohol~wine_data$quality)
kruskal.test(wine_data$alcohol~wine_data$quality)
#not
anova(lm(wine_data$alcohol~wine_data$quality))
#or
summary(aov(wine_data$alcohol~wine_data$quality))
#5)
linear_quality = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine_data)
summary(linear_quality)
#Q4
rm(list = ls())
#1)
data <-read.table("/Users/qiguangyao/Public/Courses/bioSta/20190522/第六次作业/homework-6.4-data.txt",sep=",",header = TRUE)
fit <-glm(y ~ Income + Age,data=data,family = binomial())
summary(fit)
#2)
coef(fit) 
#3)
predictdata <-data.frame(Income=c(45000),Age=c(5))
predictdata
predictdata$prob <-predict(fit,newdata = predictdata,type="response")
predictdata
#Q5
rm(list = ls())
#1)
data<-read.csv("/Users/qiguangyao/Public/Courses/bioSta/20190522/第六次作业/homework-6.5-Drivers.csv",header = T,sep = ",")
fit.full<-glm(y~x1+x2+x3, data=data, family=binomial)
summary(fit.full)
#2)
fit.reduced<-glm(y~x1, data=data, family=binomial) 
summary(fit.reduced)
#3)
testdata<- data.frame(x1=c(0,1))
testdata$prob<-predict(fit.reduced,testdata ,type='response')
testdata
#Q6
rm(list = ls())
#1)
bc<-read.csv("/Users/qiguangyao/Public/Courses/bioSta/20190522/第六次作业/homework-6.6-data.csv")
formula<-paste("diagnosis",paste(colnames(bc)[3:12],collapse="+"),sep="~")
model<-glm(formula,bc,family = binomial)
summary(model)
#2)
reduced_feature<-c('texture_mean','area_mean','smoothness_mean','concave.points_mean')
formula<-paste("diagnosis",paste(reduced_feature,collapse="+"),sep="~")
reduced.model<-glm(formula,bc,family = binomial)
summary(model)
#3)
anova(model,reduced.model,test="Chisq")

#4)
trainData<-bc[1:398,]
testData<-bc[399:569,]
reduced_feature<-c('texture_mean','area_mean','smoothness_mean','concave.points_mean')
formula<-paste("diagnosis",paste(reduced_feature,collapse="+"),sep="~")
model<-glm(formula,trainData,family = binomial)
summary(model)
pred<-predict(model,testData,type="response")
pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(pred_num, levels=c(0, 1))
y_act <- factor(ifelse(testData$diagnosis=="B",0,1))
mean(y_pred == y_act)




