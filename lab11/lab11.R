#Adam Orr
#Biometry
#Lab 11

# ---- warmup ----

fdata <- read.csv('Metab_Fake.csv')

#plot data
plot(Mass.mg~Metabolism_kW, data = fdata)

#running a correlation by r shortcut
r <- cor(fdata$Mass.mg, fdata$Metabolism_kW)

#full test
cor.test(fdata$Mass.mg, fdata$Metabolism_kW)
#to run spearman's, use method="spearman" in cor.test

#to calculate the 95% CI for r
#first convert r to Z
n <- 20
z <- 0.5*(log((1+r)/(1-r)))

#now we need standard error of z
sez <- sqrt(1/(n-3))

#zcrit
zcrit <- qnorm(0.975)

#95% CI in z terms
z.upper <- z + zcrit * sez
z.lower <- z - zcrit * sez

#convert back to r
r.upper <- (exp(2*z.upper)-1)/(exp(2*z.upper)+1)
r.lower <- (exp(2*z.lower)-1)/(exp(2*z.lower)+1)

#linear regression
mod <- lm(Metabolism_kW~Mass.mg, data=fdata)
summary(mod)

#plotting regression line
plot(data=fdata, Metabolism_kW~Mass.mg)
abline(mod)

#testing assumptions
res <- residuals(mod)
fit <- fitted(mod)

plot(res~fit)

# ---- question 1 ----

# a. Make a scatter plot of the data. State the central assumptions of
# correlation analysis and examine the plot to determine whether these
# assumptions are met.
green <- read.csv('green.csv')
plot(green$attachment~green$birds,
     main = "Attachment to Green Spaces",
     xlab = "Number of bird species",
     ylab = "Attachment")

# b. Describe the pattern of the data in words. Is the relationship positive or
# negative? Is it linear? How strong is it?
# 
# There seems to be a positive, linear relationship between the number of bird
# species and green space users' attachment. The relationship does not appear
# incredibly strong, but not weak either.
# 
# c. Calculate an estimate of Pearson’s correlation coefficient. Do this
# calculation two ways: by hand, and with the R function cor.
#
# by hand
x <- green$birds
y <- green$attachment
xbar <- mean(x)
ybar <- mean(y)
xdiff<- x - xbar
ydiff<- y - ybar
(r <- sum(xdiff*ydiff)/sqrt(sum(xdiff^2)*sum(ydiff^2)))

# using cor
cor(x,y)

# d. Use the R function cor.test to perform a t-test of the null hypothesis that
# there is no correlation. Clearly state the conclusions of the test, including
# all key information (hypotheses, significance level, test statistic, degrees
# of freedom, and P-value). Also report the 95% confidence interval of rho.

#H0: The correlation between the number of bird species in a greenspace and
#greenspace user's attachment is 0.

#HA: The correlation between the number of bird species in a greenspace and
#greenspace user's attachment is not 0.

cor.test(x,y)

# With a significance level of 0.05, we reject the null hypothesis that the 
# correlation between the number of bird species in a green space and green 
# space users' attachment is equal to 0 (t-test; t = 3.8595, df = 13, p =
# 0.002).

#The 95% confidence interval of rho is (0.349,0.904)

# ---- question 2 ----

# a. Make a scatter plot of log call frequency and log file length. Be sure that
# the independent variable is on the X-axis. Inspect the plot; does it look like
# there is a linear relationship?
katydid <- read.csv('katydid.csv')
x <- katydid$log.length
y <- katydid$log.freq
plot(x,y,
     main = "Katydid Call Frequency and File Length",
     xlab = "Log File Length (mm)",
     ylab = "Log Call Frequency (kHz)")

#There does appear to be a negative linear relationship between the log of file
#length and the log of call frequency.
#
# b. Estimate the best-fit equation for the linear regression of log call
# frequency on log file length. Do this by hand.
xbar <- mean(x)
ybar <- mean(y)
xdiff <- x - xbar
ydiff <- y - ybar

b <- sum(xdiff*ydiff)/sum(xdiff^2)
a <- ybar - (b * xbar)

#The best fit linear equation is y = 3.627 - 0.795 * x

# c. Calculate the 95% confidence interval of the regression coefficient β, by
# hand.
n <- length(x)
tcrit <- qt(c(.025,.975), n - 2)
yhat <- a + x * b
MSres <- sum((y-yhat)^2)/(n-2)
sb <- sqrt(MSres/sum(xdiff^2))
(CI <- b + tcrit * sb)

# d. Test the null hypothesis that β = 0, using a t-test. Be sure to clearly
# state your null and alternative hypotheses, as well as your conclusions.
t <- b/(sb)
#
#H0: The value of b is equal to 0.
#Ha: The value of b is not 0.
#
p <- 2*pt(t,n-2)

# With a significance of 0.05, I reject the null hypothesis that the value of b
# is equal to 0 (t-test, t = -8.39, df = 56, p = 1.8e-11).

# e. Calculate the coefficient of determination.
SSr <- sum((yhat-ybar)^2)
SSt <- sum((y-ybar)^2)
(r2 <- SSr/SSt)

# f. Re-do the regression, this time using the R functions lm and summary.
mod <- lm(y~x)
summary(mod)

# g. Overall, do the results support the use of file length to estimate call
# frequency? Explain why or why not.

#Overall, the results do support the use of file length to estimate call 
#frequency, as the slope is significantly non-zero. Additionally, the 
#coefficient of determination is somewhat large, meaning the regression explains
#a lot of the variance in frequency.

# h. Re-do the scatter plot from part a and add a regression line, using the
# command abline.
# plot(x,y,
#      main = "Katydid Call Frequency and File Length",
#      xlab = "Log File Length (mm)",
#      ylab = "Log Call Frequency (kHz)")
abline(mod)

# ---- question 3 ----

# a. Use the R functions residuals and fitted to calculate residuals and fitted
# values from the linear model you made in question 2.

#residuals
(res <- residuals(mod))

#fitted values
(fit <- fitted(mod))

# b. Plot the residuals vs. the fitted values.
plot(res~fit,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")


# c. Make a normal probability plot of the residuals.
qqnorm(res)

# d. Evaluate the plots you have made for adherence to normality, linearity, and
# homogeneity of variances.

#The residuals vs. fitted values plot shows that the residuals are somewhat 
#evenly dispersed around 0, indicating that the regression meets the assumption 
#of linearity and homogeneity. The normal probability plot shows a straight 
#line, indicating adherence to normality. Overall, there is not much evidence
#for departure from normality, linearity, or homogeneity of variances.







