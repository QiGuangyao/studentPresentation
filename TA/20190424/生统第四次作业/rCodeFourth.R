Sys.setlocale('LC_ALL','C')
#Q1
#1)

a<-c(126,125,138,128,123,138,142,116,110,108,113,140)
b<-c(160,175,177,170,175,153,168,159,160,162)
var.test(a,b)
#2)
t.test(a,b,var.equal = TRUE)
#Q2
X <- c(4.5,6.5,6,9.2,10,12,8.3)
Y <- c(4,7.2,8,14,8.8,10,11.5)
wilcox.test(X,Y,paired = TRUE,exact = FALSE)
#Q3
insure <- c(4152,4579,5053,5112,5745,6250,7081,9048,12095,14430,17220,20610,22836,48950,67200)
wilcox.test(insure,mu = 7520,conf.int = TRUE)
#Q4
#1)
# setwd('/Users/qiguangyao/Public/Courses/bioSta/20190424/生统第四次作业/')
# Data<-read.table("第四次作业Data.txt")
Data<-read.table("/Users/qiguangyao/Public/Courses/bioSta/20190424/生统第四次作业/第四次作业Data.txt",header = TRUE)
p.value<-apply(Data,1,function(x)t.test(x[1:10],x[11:20],paired = TRUE)$p.value)
DEG.pair<-rownames(Data)[p.value<0.05]
sum(p.value<0.05)
names(p.value) = rownames(Data)
names(sort(p.value)[1:10])
#2)
p.bon<-p.adjust(p.value,'bonferroni')
p.fdr<-p.adjust(p.value,'fdr')
rownames(Data)[p.bon<0.05]
rownames(Data)[p.fdr<0.05]
