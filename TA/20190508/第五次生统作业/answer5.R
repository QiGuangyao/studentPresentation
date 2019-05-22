#Q1
dat <- read.table('E:/Courses/生物医学统计学/20190508/第五次生统作业/第五次作业5.1-yield.txt',header = T,sep = "")
shapiro.test(dat[which(dat$seed==1),1])
bartlett.test(dat$yield~dat$seed)
#1
summary(aov(data = dat,yield~seed))
#2
pairwise.t.test(dat$yield,dat$seed,p.adjust.method = "none")

#Q2
#1
#一个因素，保健饮料剂量；四个水平
#2
df <- read.table(file = "E:/Courses/生物医学统计学/20190508/第五次生统作业/第五次作业5.2-data.txt",header = T)
boxplot(df)
summary(df)
#3
#4
#1. 各处理样本是相互独立的随机样本;
#2.各处理样本相应总体服从正态分布;
#3.个处理样本总体方差齐次.
#一般实验设计与实施满足第一个要求，检验要求2
for(i in 1:3){print(shapiro.test(df[,i]))}
#检验要求3
dff <- reshape2::melt(df)
## No id variables; using all as measure variables
#R中特有的因子型数据类型，有时候不加会有问题。
dff$variable <- as.factor(dff$variable) #检验
##  Bartlett test of homogeneity of variances
bartlett.test(value ~ variable,data = df)

#5
res <- aov(value ~ variable,data = dff)
summary(res)
TukeyHSD(res)

#Q3
dataQuestion3 <- read.csv('E:/Courses/生物医学统计学/20190508/第五次生统作业/question3.txt',header = T,sep = "")
summary(aov(data = dataQuestion3,increase~drugType))

#Q4
#1
dataQuestion4 <- read.table('E:/Courses/生物医学统计学/20190508/第五次生统作业/mouseWeight.csv', sep = ',', skip = 1)
colnames(dataQuestion4) <- c('A', 'B', 'Weight')
shapiro.test(dataQuestion4$Weight[dataQuestion4$A=="A1"])
shapiro.test(dataQuestion4$Weight[dataQuestion4$A=="A2"])
shapiro.test(dataQuestion4$Weight[dataQuestion4$A=="A3"])
shapiro.test(dataQuestion4$Weight[dataQuestion4$B=="B1"])
shapiro.test(dataQuestion4$Weight[dataQuestion4$B=="B2"])
bartlett.test(Weight~A,data=dataQuestion4)
bartlett.test(Weight~B,data=dataQuestion4) 
#2
fit<-aov(Weight~A+B+A:B,data = dataQuestion4)
summary(fit)
#3
TukeyHSD(fit) 

