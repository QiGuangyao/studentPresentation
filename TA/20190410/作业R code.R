pnorm(2.0, 4,1)
qnorm(0.95, 4, 1)
qnorm(0.05, 4, 1, lower.tail=FALSE)


dbinom(x = 0, size = 8, prob = 0.5)
dbinom(x = 4, size = 8, prob = 0.5)
1-pbinom(q=5, size=8, prob=0.5)


x <- rnorm(n=1000, mean=100, sd=8)
par(mfrow=c(1,2))
hist(x)
boxplot(x)

t.test(x, mu=100)
wilcox.test(x, mu=100)
