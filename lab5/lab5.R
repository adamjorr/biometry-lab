#Adam Orr
#Biometry
#Lab 5

# ---- Warmup ----
#sex ratio of hummingbirds
total <- 20
female <- 7

#is this a 50/50 sex ratio (1:1)
phat <- female/total

#step 1 : state hypotheses
#h0: sex ratio is equal
#ha: sex ratio is not equal

#step 2 : state significance level
#alpha = .05

#step 3 : define test statistic
#binomial exact test
#the test statistic is x, # of females from sample of 20 offspring

#step 4 : calculate test statistic
x <- 7

#step 5 : calculate p-value and state conclusion
n <- 20
p_null <- .5

#calc p-value
pbinom(x,n,p_null) * 2

barplot(dbinom(0:20,n,p_null),
  names.arg=0:20,
  ylab="prob",
  xlab="num females",
  col=c(rep("red",8),rep("gray",5),rep("red",8)))

#conclusion statement
#we fail to reject the null hypothesis that the sex ratio is 1:1 at a significance
#level of .05 (binomial test: p = .263, n = 20)

#binomial test in R
binom.test(x,n,p_null)

# ---- question 1 ----
#calculate the following values:
numplants <- 24
p_wrinkled <- 1/4

#a. The probability that exactly 8 plants will have wrinkled peas.
dbinom(8, numplants, p_wrinkled)

#b. The probability that 8 or fewer plants will have wrinkled peas.
pbinom(8, numplants, p_wrinkled)

#c. The probability that 12 or more plants will have wrinkled peas.
pbinom(11, numplants, p_wrinkled, lower.tail = FALSE)

#d. The 0.025 quantile of the number of plants with wrinkled peas.
qbinom(.025, numplants, p_wrinkled)

# ---- question 2 ----
#experiment parameters
total <- 25
nfemales <- 15
nmales <- 10

# a. Use the data to estimate the probability p that a reproductive offspring is female.
(phat <- nfemales / total)

# b. Use the Agresti-Coull method to calculate the 95% confidence interval for p.
p_prime <- (nfemales + 2)/(total + 4)
s_pp <- sqrt((p_prime * (1 - p_prime))/(total + 4))
c(p_prime - 2 * s_pp, p_prime + 2 * s_pp)

# c. Based on the confidence interval you calculated, briefly explain whether it is plausible that the true sex ratio is 1:1 (that is, p = 0.5).
# It is plausible that the true sex ratio is 1:1, as the confidence interval includes .5 .

# d. Now carry out a formal test of the hypothesis that the sex ratio is 1:1. Perform this test by hand in R, explicitly carrying out the five steps of hypothesis testing:
# 1) State the null and alternative hypotheses.
# H0: The sex ratio is .5
# Ha: The sex ratio is not equal to .5

# 2) State the significance level.
# alpha = 0.05

# 3) Define the test statistic.
#  The test stastistic is X, the number of females in a sample of 25 offspring
p_null <- 0.5

# 4) Calculate the test statistic from the data.
nfemales

# 5) Calculate the P-value and use it to make a decision about the hypothesis under test.
pbinom(nfemales - 1, total, 0.5, lower.tail = FALSE) * 2 
#We fail to reject the null hypothesis that the sex ratio is .5 at a significance level of .05 (Binomial test: p = .4244, n = 25)

# e. Perform the same test using the R command binom.test. Show the complete output of the test.
binom.test(nfemales, total)

# f. Explain whether the result of your hypothesis test consistent with the confidence interval that you calculated in part b).
#The result of the hypothesis test is consistent with the confidence interval.
#The confidence interval includes .5, which we are not able to reject with the hypothesis test.

# ---- question 3 ----
total <- 18
dog_food <- 2
people_food <- total - dog_food
# a. What proportion of participants chose dog food as their favorite?
dog_food / total

# What proportion are expected to choose dog food, assuming no preference among the five items (i.e., they choose a favorite randomly)?
(p_null <- 1/5)

# b. Use binom.test to test the hypothesis that people have a preference between human food and dog food. Show the complete output of the test.
binom.test(dog_food,total,p_null)

#Include a complete statement of your conclusions
#We do not reject the null hypothesis that people have a preference between human food and dog food, at a significance level of .05 (Binomial test : p = 0.555, n = 18).

# c. binom.test reports a confidence interval. Briefly describe what this interval means, and state whether it is consistent with the results of your hypothesis test.
#The confidence interval has a 95% chance of including the true mean of the number of people who chose dog food as their favorite. It is consistent with the results of the hypothesis test because .2 is included in the confidence interval.

# ---- question 4 ----
total <- 24
on_x_chr <- 11
p_null <- 1/4

# a. Use these data and binom.test to test the theory that spermatogenesis genes are found disproportionately on the X chromosome.
binom.test(on_x_chr, total, p_null)

# Report your results as described for exercise 3b.
#We reject the null hypothesis that the spermatogenesis genes are distributed proportionally on the X chromosome, at a significance level of .05 (Binomial test : p = 0.0304, n = 24).

# b. Briefly explain whether the confidence interval reported by binom.test is consistent with the results of your hypothesis test.
#The confidence interval reported by binom.test is consistent with the results of the hypothesis test because it doesn't include .25.





