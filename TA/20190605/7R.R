rm(list = ls())
data <- read.table("/Users/qiguangyao/Public/Courses/bioSta/20190605/生统第七次作业/7.1-stocks.txt", row.names = 1, header = T)
pca <- princomp(data, cor = T)
summary(pca)
biplot(pca)
loadings(pca)
print(loadings(pca))

rm(list = ls())
library(dplyr)
iris<-select(iris,-Species)
iris.pca<-princomp(iris,cor = T)
summary(iris.pca,loadings = T)
new.iris<-as.data.frame(iris.pca$scores[,1:3])

dat <- iris[,1:4]
hc = hclust(dist(dat))
plot(hc)



rm(list = ls())
data <- read.csv('/Users/qiguangyao/Public/Courses/bioSta/20190605/生统第七次作业/7.5dataPoints.csv',header = T,sep = ',')
data.pca <- prcomp(data,center = TRUE,scale = TRUE)
data.pca
data.pca$x[,1]
var(data.pca$x[,1])

PC1 <- c(sqrt(2)/2,sqrt(2)/2)
transformData <- data.pca$x[,1]
transformData[1]*PC1
transformData[2]*PC1
transformData[3]*PC1
