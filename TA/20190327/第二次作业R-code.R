#1
data <- c(14,24,24,23,15,19,17,21,15,19)
s <- (max(data)-min(data))/4
#2
lamda <- 0.05
p <- (1-exp(-15*lamda)) - (1-exp(-5*lamda))
#3
p <- .6
n <- 40
y <- 15
mean_Y <- n*p
sigma_Y <- sqrt(n*p*(1-p))
bandoury <- mean_Y - 3*sigma 
bandoury <= y
