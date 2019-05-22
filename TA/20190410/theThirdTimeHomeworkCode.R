rm(list = ls())
#Q1
#a
a<-c(6802,5730,5823,5915,5774,5880,5870,5773,5830,5841,5763,5851,5789,5796,5818,5685,5602,5841,5723,5757)
b<-c(5884,5871,5797,5957,5803,5862,5814,5885,5856,5940,5945,5803,5864,5851,5714,5943,5830,5858,5922,5866)
n1<-length(a)
n2<-length(b)
var1<-var(a)
var2<-var(b)
F<-var1/var2
p<-2*(1-pf(F,df1=(n1-1),df2=(n2-1)))
#b1
a_b <- c(a,b)
x <- c(1:40)
length(x)
n <- 1000
diff <- 0
for(i in 1:n) {x <- sample(x);diff[i] <- mean(a_b[x[1:20]]) - mean(a_b[x[21:40]]);}
hist(diff, xlab= "Test statistic", main="Histogram of test statistic", cex.lab= 2, cex.axis=1.5, cex.main=2)
diff_a_b <- mean(a)-mean(b)
arrows(diff_a_b, 50, diff_a_b, 0, col="red")
text(diff_a_b, 52, "observed test statistic", col="red")
p_value <- length(diff[diff>=diff_a_b])/1000
p_value
#b2
t.test(a,b,var.equal=FALSE)
#Q2
#
rm(list = ls())
pnorm(2.0, 4,1)
qnorm(0.95, 4, 1)
qnorm(0.05, 4, 1, lower.tail=FALSE)
#
dbinom(x = 0, size = 8, prob = 0.5)
dbinom(x = 4, size = 8, prob = 0.5)
1-pbinom(q=5, size=8, prob=0.5)
#
x <- rnorm(n=1000, mean=100, sd=8)
par(mfrow=c(1,2))
hist(x)
boxplot(x)
#
t.test(x, mu=100)

#Q3
rm(list = ls())
#a
getwd()
#b
setwd("/Volumes/KINGSTON/bioStaDis/20190410")
data <- read.table("homework3_data.txt", header = T)
#c
boxplot(data$PM2.5_score ~ data$Month)
#d
## d)
data_march <- subset(data, Month == "March")
# or
data_march <- data[data$Month == "March",]
# or ....

#Q4
rm(list = ls())
#1
setwd("/Volumes/KINGSTON/bioStaDis/20190410")
data <- read.table("第三次作业homework4_data.csv", header = T,sep = ',')
m <- mean(data$Birthweight)
s <- sd(data$Birthweight)
n <- 20
p.upper <- pt((126.0-118)/(s/sqrt(n)),n-1)
p.lower <- pt((100.0-118)/(s/sqrt(n)),n-1)
p <- p.upper-p.lower
p
#2
left <- m-qt(0.975,n-1)*s/sqrt(n)
right <- m+qt(0.975,n-1)*s/sqrt(n)
left;right
#3
t <- (m-118)/s*sqrt(n)
p <- 1-pt(t,n-1)
p
#[1] 0.2328299
#P=0.2328299 >0.05
##使用t.test()##
t.test(data$Birthweight, alternative = "greater", mu= 118)
# One Sample t‐test
#data: data$Birthweight
#t = 0.74454, df = 19, p‐value = 0.2328
#alternative hypothesis: true mean is greater than 118
#95 percent confidence interval:
# 115.3552 Inf
#sample estimates:
#mean of x
# 120

#4
#使用双尾检测
#又因为t>0
p <- (1-pt(t,n-1))*2
p
#[1] 0.4656597
#P=0.4656597>0.05
##使用t.test()##
t.test(data$Birthweight, mu = 118)
# One Sample t‐test
#data: data$Birthweight
#t = 0.74454, df = 19, p‐value = 0.4657
#alternative hypothesis: true mean is not equal to 118
#95 percent confidence interval:
# 114.3777 125.6223
#sample estimates:
#mean of x
# 120

#5
#(5) 6分 R代码4分 结果2分
power <- pt(qt(0.025,n-1)+abs(m-118)/s*sqrt(n),n-1)
power

#6
#使用t‐test计算样本量时可使用z‐test正态分布进行估算
n <- (qnorm(1-0.05)+qnorm(1-0.01/2))^2*s^2/(m-118)^2
n
#[1] 642.7163
# n=643

#or 使用pwr包
install.packages('pwr')
library(pwr)
pwr.t.test(d=abs(m-118)/s,sig.level=0.01,power=1-0.05,type="one.sample",alternative="two.sided")
# One‐sample t test power calculation
#
# n = 646.0381
# d = 0.1664842
# sig.level = 0.01
# power = 0.95
# alternative = two.sided
# n=647




