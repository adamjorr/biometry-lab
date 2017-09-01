#Adam Orr
#Biometry
#Lab 2
# ---- warmup ----
values <- seq(1,20)
rolls <- sample(values, size = 1000, replace = TRUE)
mean(rolls)

# ---- question 1 ----
nfemales <- 3
total <- 9
p <- .5

(combinations <- choose(total, nfemales))
(p_each <- p ^ (nfemales) * (1 - p) ^ (total - nfemales))
(tot_p <- combinations * p_each)

# ---- question 2 ----
nfemales <- seq(0,9)
total <- rep(9,10)
p <- rep(.5,10)

(combinations <- choose(total, nfemales))
(p_each <- p ^ (nfemales) * (1 - p) ^ (total - nfemales))
(tot_p <- combinations * p_each)

# ---- question 3 ----
library('tidyverse')
nfemales <- seq(0,9)
total <- 9
p <- .5

(prob <- dbinom(nfemales, size = max(numfemales), .5))
barplot(prob, names.arg = nfemales, xlab = "Number of Females", ylab = "Probability", ylim = c(0, max(prob + .05)))
(mu <- sum(nfemales * prob))
(mu <- total * p)
(variance <- sum((nfemales - mu)^2 * prob))
(sdev <- sqrt(variance))
(variance <- mu * (1 - p))
(sdev <- sqrt(variance))

mn <- function(total, p){ total * p }
va <- function(mn, p){ mn * (1 - p)}
ps <- c(0.1,0.3,0.5,0.7,0.9)
means <- mn(rep(total,length(ps)),ps)
variances <- va(means, ps)
df <- as_tibble(data.frame(p = ps,mean = means, variance = variances))

# ---- question 4 ----
par(mfrow=c(2,1))
barplot(prob, xlab = "Number of Females", ylab = "Probability")
barplot(cumsum(prob), xlab = "Number of Females", ylab = "Cumulative Probability")

# ---- question 5 ----
total <- 9
p <- .5
dbinom(4,9,p)
pbinom(7,9,p)
1 - pbinom(3,9,p)
pbinom(5,9,p)
(1 - pbinom(6,9,p)) + pbinom(2,7,p) #7 or more females + 7 or more males

# ---- question 6 ----
qbinom(.25,9,.5)
qbinom(.05,9,.5)
qbinom(.95,9,.5)
qbinom(.5,9,.5)

# ---- question 7 ----


