#Adam Orr
#Biometry
#Lab 3

# ---- warmup ----
#functions
#dnorm() probability density
#pnorm() 
#qnorm()
#rnorm()

#time hummingbird spends at feeder
#mean - 30 s
#var - 100 s

#parameters
u <- 30
o2 <- 100
o <- sqrt(o2)

dnorm(10,u,o) #density of 10 sec at feeder
pnorm(10,u,o) #prob at feeder for <10 sec
qnorm(.25,u,o) #first quartile feeding time

(p <- pnorm(10,u,o))
qnorm(p, u, o)

(q <- qnorm(0.25,u,o))
pnorm(q, u, o)

# ---- warmup - plotting ----
q <- seq(u-4*o,u+4*o, by = .1)
pdf <- dnorm(q, u, o)

#plotting pdf
plot(pdf ~ q, type = "l")

#plotting cdf
cdf <- pnorm(q,u,o)
plot(cdf~q, type = 'l')

# ---- warmup - adding another line ----
second_u <- 20
second_o2 <- 150
second_o <- sqrt(second_o2)
second_pdf <- dnorm(q, second_u, second_o)

#plotting both
plot(pdf ~ q, type = 'l')
lines(second_pdf ~ q, type = 'l', col = 'red')

xrange <- range(q)
yrange <- range(pdf, second_pdf)

plot(pdf~q, type = 'l', xlim = xrange, ylim = yrange)
lines(second_pdf~q, type = 'l', col = 'red')
legend("topright",
       legend = c("u = 30, o2 = 100", "u = 20, o2 = 150"),
       col = c('black','red'),
       lty = 1,
       bty = "n")
# ---- warmup - standard normal ----
qnorm(.25) #r will assume std normal if no u or o provided
pnorm(1)

# ---- question 1 ----
#set parameters
mu <- 100
sigma_squared <- 400
sigma <- sqrt(sigma_squared)

#plot the probability density function of lizard lengths
q <- seq(mu - 4 * sigma, mu + 4 * sigma, by = .1)
pdf <- dnorm(q,mu,sigma)
plot(pdf~q, type = 'l',
     xlab = "Length (cm)",
     ylab = "Probability Density",
     main = "Distribution of Lizard Lengths")

#plot the cumulative distribution function of lizard lengths
cdf <- pnorm(q, mu, sigma)
plot(cdf~q, type = 'l',
     xlab = "Length (cm)",
     ylab = "Cumulative Probability",
     main = "Cumulative Probability of Lizard Lengths")

# ---- question 2 ----
#set parameters
mu <- 100
sigma_squared <- 400
sigma <- sqrt(sigma_squared)
#what is the probability density for a length of 75 cm?
dnorm(75, mu, sigma)

#what is the probability that a lizard will be less than
# or equal to 75 cm?
pnorm(75, mu, sigma)

#greater than 120 cm?
1 - pnorm(120, mu, sigma)

#between 95 and 115 cm?
pnorm(115, mu, sigma) - pnorm(95, mu, sigma)

#at least 40 cm different from the mean?
2 * pnorm(mu - 40, mu, sigma)

#closer than 1.3 sigma to the mean?
pnorm(mu + 1.3 * sigma, mu, sigma) - pnorm(mu - 1.3 * sigma, mu, sigma)

#further than 1.5 sigma from the mean?
2 * pnorm(mu - 1.5 * sigma, mu, sigma)

#further than 0.7 sigma from the mean?
2 * pnorm(mu - .7 * sigma, mu, sigma)

#what are the quartiles of the distribution?
#1st quartile
qnorm(.25, mu, sigma)

#2nd quartile
qnorm(.5, mu, sigma)

#3rd quartile
qnorm(.75, mu, sigma)

#4th quartile
qnorm(1, mu, sigma)

#2/3 of observations are expected to lie below what value?
qnorm(2/3, mu, sigma)

#80% of observations are expected to lie above what value?
qnorm(1 - .8, mu, sigma)

# ---- question 3 ----
#set parameters
mu <- rep(100,3)
sigma_squared <- c(100,400,625)
sigma <- sqrt(sigma_squared)
q <- seq(min(mu) - 4 * max(sigma), max(mu) + 4 * max(sigma), by = .1)

#calculate pdfs
pdfs <- sapply(sigma, FUN = function(x){dnorm(q,mu,x)})

#plot 3 PDFs on the same graph, each with a mean of 100,
#but with different variances. plot each line in a different
#color
par(mfrow=c(2,1)) #put 2 plots on one figure
plot(pdfs[,1]~q, type = 'l',
     xlab = "Tail length (cm)",
     ylab = "Probability Density",
     main = "Probability Density of Distributions\nWith Varying Variance")
lines(pdfs[,2]~q, col = 'red')
lines(pdfs[,3]~q, col = 'blue')

#add legend
legend("topright",
       legend = c("u = 100, o2 = 100", "u = 100, o2 = 400", "u = 100, o2 = 625"),
       col = c('black','red','blue'),
       lty = 1,
       bty = "n",
       cex = .8)

#the second plot should show three corresponding CDFs
#calculate the CDFs
cdfs <- sapply(sigma, FUN = function(x){pnorm(q,mu,x)})
plot(cdfs[,1]~q, type = 'l',
     xlab = 'Tail Length (cm)',
     ylab = "Probability",
     main = "Cumulative Probability of Distribution\nWith Varying Variance")
lines(cdfs[,2]~q, col = 'red')
lines(cdfs[,3]~q, col = 'blue')

#add legend
legend("bottomright",
       legend = c("u = 100, o2 = 100", "u = 100, o2 = 400", "u = 100, o2 = 625"),
       col = c('black','red','blue'),
       lty = 1,
       bty = "n",
       cex = .8)

# ---- question 4 ----
#if the mean height of British men is 177 cm with a standard
#deviation of 7.1 cm, what proportion of British men are
#excluded from being spies by this height restriction?
#assume height follows a normal distribution

#set parameters for men
men_mu <- 177
men_sigma <- 7.1

#proportion excluded 
(p_men_excluded <- 1 - pnorm(180, men_mu, men_sigma))

#the mean height of british women is 163.3 cm,
#with a standard deviation of 6.4 cm. assuming a normal distribution
#of female height, what fraction of women meet MI5's  height standard?

#set parameters for women
women_mu <- 163.3
women_sigma <- 6.4

#fraction of women that meet MI5's height standard
pnorm(173, women_mu, women_sigma)

#imagine that mi5 wants to change its maximum height for
#female spies. its goal is to exclude the same proportion
#of women as men.
#What should the new maximum height for women be?
#(round your answer to the nearest centimeter)
(women_q <- qnorm(1 - p_men_excluded, women_mu, women_sigma))
round(women_q, 0)

#Sean Connery, the original James Bond, is 183 cm tall.
#By how many standard deviations does he exceed the height
# limit for spies?
(183 - men_mu) / (men_sigma)



# ---- question 5 ----
#What is the probability that a normal random variable will
#have a value within 1 standard deviation of the mean?
1 - 2 * pnorm(-1)

#What is the probability that it will be within 5 standard
#deviations of the mean?
1 - 2 * pnorm(-5)

#Fill in the blank: A normal random variable has a 50%
#probability of lying within ___ standard deviations of
#the mean
abs(qnorm(.25))

#Fill in the blank: A normal random variable has a 95%
#probability of lying within ___ standard deviations of
#the mean
abs(qnorm(.025))

#Fill in the blank: A normal random variable has a 99%
#probability of lying within ___ standard deviations of
#the mean
abs(qnorm(.005))


