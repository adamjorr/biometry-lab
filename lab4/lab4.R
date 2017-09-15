#Adam Orr
#Biometry
#Lab X
# --- warmup ---
#hummingbird attack rates at feeders
u <- 10
o2 <- 25
o <- sqrt(o2)

#generating data
data <- rnorm(30,u,o)

#estimating parameters in R
mean(data)
var(data)
sd(data)

#creating a histogram
hist(data, xlab="Number of attacks",
     main = "Histogram of hummingbird attacks",
     ylim=c(0,15))

#forcing hstogram to show density not freq
hist(data, xlab="Number of attacks",
     main = "Histogram of hummingbird attacks",
     freq=FALSE, ylim=c(0,0.1))

x<-seq(u-4*o,u+4*o,by=.1)
pdf <- dnorm(x,u,o)
lines(pdf~x, col='red')

# ---- question 1 ----
#import the data
setwd("~/code/biometry-lab/lab4")
df <- read.csv('lizard.csv')
(lengths <- df$length)

#estimate the mean of length by direct calculation
(xbar <- sum(lengths)/length(lengths))

#estimate the mean with R's built in function
mean(lengths)

#estimate the variance of length by direct calculation
(s2 <- sum((lengths  - xbar)^2)/(length(lengths)-1))

#estimate the variance with R's built in function
var(lengths)

#estimate the standard deviation of length by direct calculation
(s <- sqrt(s2))

#estimate the standard deviation with R's built-in function
sd(lengths)

#estimate the standard error of the mean length
(sem <- s / sqrt(length(lengths)))

#and calculate the approximate 95% confidence interval
c(xbar - 2*sem, xbar + 2 * sem)

# ---- question 2 ----
#make a histogram of the lizard lengths
hist(lengths, xlab = "Lizard Length",
     ylab = "Probability Density",
     main = "Histogram of Lizard Length",
     freq = FALSE, ylim = c(0,.03))

#add a curve to the histogram showing your estimate of the pdf
x<-seq(xbar-4*s, xbar+4*s, by = .1)
pdf <- dnorm(x,xbar,s)
lines(pdf~x, col='red')

# ---- question 3 ----
#redraw plot
hist(lengths, xlab = "Lizard Length",
     ylab = "Probability Density",
     main = "Histogram of Lizard Length",
     freq = FALSE, ylim = c(0,.12))
lines(pdf~x, col = 'red')

#add another curve to the histogram showing the pdf of the average of 15 lizard lengths
pdf15 <- dnorm(x,xbar, s / sqrt(15))
lines(pdf15~x, col = 'blue')

#add one more curve showing the pdf of the average of 25 lizard lengths
pdf25 <- dnorm(x,xbar, s / sqrt(25))
lines(pdf25~x, col = 'green')

#add a legend
legend("topright",
       legend = c("PDF", expression(paste(bar(X), ", n = 15")), expression(paste(bar(X),", n = 25"))),
       col = c('red','blue','green'),
       lty = 1,
       bty = "n")

# ---- question 4 ----
#actual population parameters
mu <- 12
sigma <- 2

#what is the probability that a randomly sampled male will have a horn less than 11 mm long?
pnorm(11,mu,sigma)

#What is the probability that the average length of a random sample of 10 horns will be less than 11 mm?
pnorm(11,mu,sigma/sqrt(10))

#What is the probability that the average length of a random sample of 10 horns will be greater than 12.5 mm?
1 - pnorm(12.5, mu, sigma/sqrt(10))

#What is the probability that the average length of a random sample of 50 horns will be greater than 12.5 mm?
1 - pnorm(12.5, mu, sigma/sqrt(50))

#What are the 0.025 and 0.975 quantiles of average horn length, for a sample of 10?
#0.025 quantile
qnorm(.025, mu, sigma/sqrt(10))

#0.975 quantile
qnorm(.975, mu, sigma/sqrt(10))

# ---- question 5 ----
mu <- 6.2
sigma2 <- 0.25
sigma <- sqrt(sigma2)

#Generate a sample of 10 randomly samples cone weights.
(sample <- rnorm(10,mu,sigma))

#Use your sample to estimate the mean, variance, and standard deviation using R's built-in functions
#mean
mean(sample)
#variance
var(sample)
#standard deviation
sd(sample)

#Use your sample to estimate the standard error of the mean
(sem <- sd(sample)/sqrt(length(sample)))

#and calculate the approximate 95% confidence interval
c(mean(sample) - 2 * sem, mean(sample) + 2 * sem)

# ---- question 6 ----


