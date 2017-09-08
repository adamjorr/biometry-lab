#Adam Orr
#Biometry
#Lab 2
# ---- warmup ----
values <- seq(1,20)
rolls <- sample(values, size = 1000, replace = TRUE)
mean(rolls)

# ---- question 1 ----
nfemales <- 3 #number of females
total <- 9 #total size of sample
p <- .5 #probability of female

(combinations <- choose(total, nfemales)) #calculate number of combinations
(p_each <- p ^ (nfemales) * (1 - p) ^ (total - nfemales)) #calculate probability of each combination
(tot_p <- combinations * p_each) #probability for 3 females

# ---- question 2 ----
nfemales <- seq(0,9) #number of females
total <- rep(9,10) #total size of sample
p <- rep(.5,10) #probability of female 

(combinations <- choose(total, nfemales)) #calculate number of combinations
(p_each <- p ^ (nfemales) * (1 - p) ^ (total - nfemales)) #calculate probability of each combination
(tot_p <- combinations * p_each) #probability for all combinations

# ---- question 3 ----
nfemales <- seq(0,9) #number of females
total <- 9 #total sample size
p <- .5 #probability of female

(prob <- dbinom(nfemales, size = total, .5)) #probability of each outcome
barplot(prob, #
        names.arg = nfemales,
        xlab = "Number of Females",
        ylab = "Probability",
        ylim = c(0, max(prob + .05)),
        main = "Distribution of Females")
(mu <- sum(nfemales * prob)) #calculate mean with general definition
(mu <- total * p) #calculate mean with shortcut formula
(variance <- sum((nfemales - mu)^2 * prob)) #calculate variance with general definition
(sdev <- sqrt(variance)) #take square root to get std. deviation
(variance <- mu * (1 - p)) #calculate variance with shortcut definition
(sdev <- sqrt(variance)) #take square root to get std. deviation

mn <- function(total, p){ total * p } #function to calculate mean
va <- function(mn, p){ mn * (1 - p)} #function to calculate variance
ps <- c(0.1,0.3,0.5,0.7,0.9) #probabilities to test
means <- mn(rep(total,length(ps)),ps) #means of all probabilites
variances <- va(means, ps) #variance for all probabilities
(table <- data.frame(p = ps,mean = means, variance = variances)) #table showing mean and variance for each p

# ---- question 4 ----
par(mfrow=c(2,1)) #put 2 plots on one figure
barplot(prob, xlab = "Number of Females", ylab = "Probability", ylim = c(0, max(prob + .05))) #plot probability distribution
barplot(cumsum(prob), xlab = "Number of Females", ylab = "Cumulative Probability") #plot cumulative probability distribution

# ---- question 5 ----
p <- .5 #probability of a female
dbinom(4,9,p) # exactly 4 females
dbinom(7,9,p) # exactly 7 females
pbinom(7,9,p) # 7 or fewer females
1 - pbinom(3,9,p) # 4 or more females
pbinom(5,9,p) # 4 or more males
(1 - pbinom(6,9,p)) + pbinom(2,7,p) #7 or more females + 7 or more males

# ---- question 6 ----
qbinom(.25,9,.5) # first quartile
qbinom(.05,9,.5) # 5th percentile
qbinom(.95,9,.5) # 95th percentile
qbinom(.5,9,.5) # median

# ---- question 7 ----
(vals <- rbinom(100,9,.5)) #100 simulated values
mean(vals) #mean of simulated values

