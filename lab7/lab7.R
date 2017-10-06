#Adam Orr
#Biometry
#Lab 7
# ---- warmup ----
#load data
setwd("~/code/biometry-lab/lab7")
feeder <- read.csv('feeder.csv')

#convert to table
obs <- table(feeder)

#making a mosaic plot
mosaicplot(obs,
           col = c("skyblue","red"),
           xlab = "Species",
           ylab = "Feeder Color",
           main = "")

#expected values
#get sums for row, column, and total
blue <- sum(obs[,1]) #[,1] for first column
red <- sum(obs[,2])
allen <- sum(obs[1,])
anna <- sum(obs[2,])
costa <- sum(obs[3,])
grand <- sum(obs)

#calc exp values
exp_AlB <- (blue * allen) / grand
exp_AlR <- (red * allen) / grand
exp_AnB <- (anna * blue) / grand
exp_AnR <- (anna * red) / grand
exp_CoB <- (costa * blue) / grand
exp_CoR <- (costa * red) / grand

#put values into a table
exp <- obs #copy table
exp[1,1] <- exp_AlB
exp[1,2] <- exp_AlR
exp[2,1] <- exp_AnB
exp[2,2] <- exp_AnR
exp[3,1] <- exp_CoB
exp[3,2] <- exp_CoR

#expected values meet cochran's rules
#chisq
chisq.test(obs, correct = F)

# ---- fisher's exact test warmup ----
feeder2 <- read.csv("feeder2.csv", row.names = 1)
fisher.test(feeder2)

# ---- question 1 ----
setwd("~/code/biometry-lab/lab7")

#a. Read the data into R and convert them into a table
yawns <- read.csv('yawn.csv')
obs <- table(yawns)

# b. Make a mosaic plot of the results. Inspect the plot for evidence of an
# association between eye visibility and yawning contagion.
mosaicplot(obs,
           col = c("skyblue","red"),
           xlab = "Species",
           ylab = "Feeder Color",
           main = "")
#From the plot, it appears there may be an association.
#When the eyes are covered, there are slightly less people yawning.

# c. Test for an association between eye visibility and yawning contagion. Use 
# an appropriate test and clearly state the conclusions of the test. Do this 
# test “by hand” in R.

#compute marginals
covered <- sum(obs[1,])
uncovered <- sum(obs[2,])
noyawn <- sum(obs[,1])
yawn <- sum(obs[,2])
grand <-sum(obs)

#compute expected values
exp_covered_noyawn <- covered * noyawn / grand
exp_covered_yawn <- covered * yawn / grand
exp_uncovered_noyawn <- uncovered * noyawn / grand
exp_uncovered_yawn <- uncovered * yawn / grand

#put expected values into table
exp <- obs
exp[1,1] <- exp_covered_noyawn
exp[1,2] <- exp_covered_yawn
exp[2,1] <- exp_uncovered_noyawn
exp[2,2] <- exp_uncovered_yawn

#calculate chi-sq
alpha <- 0.05
x2 <- sum((obs - exp)^2/exp)
df <- 1
(p <- 1 - pchisq(x2, 1))

# We fail to reject the null hypothesis that eye visibility is independent of 
# yawning contagion with an alpha of 0.05 (Chi-square: X2 = 2.4107, df = 1, p = 
# 0.1205)

# A chi-squared test was performed because the data satisfies Cochran's rules

# d. Repeat the test using an appropriate R function.
chisq.test(obs, correct = F)

# ---- question 2 ----
# create data frame of data
obs <- data.frame(fed = c(0,5), unfed = c(6,3),
                  row.names = c('full','hungry'))

# calculate marginals
full <- sum(obs[1,])
hungry <- sum(obs[2,])
fed <- sum(obs[,1])
unfed <- sum(obs[,2])
total <- sum(obs)

# calculate expected values
exp_full_fed <- full * fed / total
exp_hungry_fed <- hungry * fed / total
exp_full_unfed <- full * unfed / total
exp_hungry_unfed <- hungry * unfed / total

# create expected table
exp <- obs
exp[1,1] <- exp_full_fed
exp[1,2] <- exp_full_unfed
exp[2,1] <- exp_hungry_fed
exp[2,2] <- exp_hungry_unfed

# check whether Cochran's rules are satisfied by inspecting the expected values
exp
# Since Cochran's rules are unsatisfied, a Fisher's Exact Test will be better
# than a Chi-squared test for this data.

fisher.test(obs)

# We reject the null hypothesis that regurgitative feeding is independent of
# hunger with an alpha of 0.05 (Fisher's Exact Test: p = .03097)


