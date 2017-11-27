#Adam Orr
#Biometry
#Lab 10

# ---- warmup ----
fakedata <- read.csv('worms.csv')

#make a stripchart to visualize data
stripchart(lifespan~treatment, method = "jitter", data = fakedata, vertical = T)

#using tapply
avgs <- tapply(fakedata$lifespan, fakedata$treatment, mean)

points(avgs, pch=15, col = 'red') #add average points to plot

#r short cut
modl <- aov(lifespan~treatment, data = fakedata)
summary(modl)

#tukey test - r shortcut
#make sure your tukey values are positive!!
TukeyHSD(modl)

#adding stuff to a CI plot
library(gplots)

n <- tapply(fakedata$lifespan, fakedata$treatment, length)
se <- sqrt(17.75/n) #value from ANOVA table (summary modl)

tcrit <- qt(0.975, 41)

CI <- tcrit * se #interval length

#plot ci
plotCI(avgs, uiw = CI, xaxt = "n", ylim=c(15,30))

#add back x axis
axis(1, at=1:4, labels = c("adult","both","larva","water"))

#add group labels from Tukey test
text(x = 1:4, y = avgs+CI+1, labels=c("a,b","a","b,c","c"))

# ---- question 1 ----
suppressPackageStartupMessages(library(gplots))

setwd("~/code/biometry-lab/lab10/")
crab <- read.csv("crabs.csv")
means <- tapply(crab$rate, crab$group, mean)

# Make a strip chart of the data.
stripchart(rate~group,
           data = crab,
           method = "jitter",
           vertical = T,
           main = "Crab Temperature Loss",
           xlab = "Group",
           ylab = "Rate (Degrees C / log minute)")
points(means, pch=15, col = 'red') #add average points to plot
legend('topright',legend = c("Data","Group average"), col= c('black','red'),pch=c(0,15))

# ---- question 2 ----
# Do an ANOVA to test for any differences in mean rate of
# heat gain among the groups.
df_treatment <- length(means) - 1
df_error  <- length(crab$rate) - length(means)
total_mean <- mean(crab$rate)
SS_treatment <- sum(tapply(crab$rate, crab$group, FUN=function(x){sum(length(x)*(mean(x) - total_mean)^2)}))
SS_error <- sum(tapply(crab$rate, crab$group, FUN=function(x){sum((x - mean(x))^2)}))
MS_treatment <- SS_treatment/df_treatment
MS_error <- SS_error / df_error
fval <- MS_treatment/MS_error
pval <- 1 - pf(fval, df_treatment, df_error)



# ---- question 3 ----
# 3. Re-do the ANOVA using R’s aov command.
mod <- aov(rate~group, data = crab)
summary(mod)

# ---- question 4 ----
# a. Estimate the group’s mean rate of heat gain.
means

# b. Calculate the 95% confidence interval of the mean.
se <- sqrt(MS_error/length(crab$rate))
tcrit <- qt(c(.025,.975),df_error)
cis <- lapply(means, FUN = function(x){x + tcrit * se}) #confidence interval for each grp
cis

# ---- question 5 ----
# Now determine perform pairwise comparisons to test which groups differ from which.
comparisons <- combn(unique(crab$group),2,simplify = F)

# Perform Tukey-Kramer tests on all possible comparisons
diffs <- sapply(comparisons, FUN=function(x){means[x[2]] - means[x[1]]}) #for each comparison, get difference in mean
SE_tukeys <- sapply(comparisons, FUN=function(x){sqrt(0.5 * (MS_error/sum(crab$group == x[1])+MS_error/sum(crab$group == x[2])))}) #for each comparison, calculate the tukey SE. the sum of a boolean vector is the number of TRUE values.
qs <- abs(diffs)/SE_tukeys
pvals <- 1 - ptukey(qs,length(unique(crab$group)),df_error)
conclusions <- ifelse(pvals < 0.05, "Reject", "Fail to Reject")

tukeytable <- data.frame(comparison = sapply(comparisons, paste, collapse = " - "), diff = diffs, q = qs, p = pvals, conclusion = conclusions)

# ---- question 6 ----
# Repeat these Tukey-Kramer tests using R’s TukeyHSD command.
TukeyHSD(mod)

# ---- question 7 ----
# Make an error bar plot, and label it to indicate the
# Tukey-Kramer test results. For your measure of precision, use the 95%
# confidence interval of the mean for each group.
tcrit <- qt(.975,df_error)
cilen <- tcrit * se #interval length

#plot ci
plotCI(means, uiw = cilen, xaxt = "n", ylim=c(0.5,2), xlab = "Group", ylab = "Heat Gain (Degrees C / minute)", main = "Crab Temperature Loss")

#add back x axis
axis(1, at=1:4, labels = names(means))

#add group labels from Tukey test
text(x = 1:4, y = means+cilen+.1, labels=c("a","b","a","b"))









