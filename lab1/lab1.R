#Adam Orr
#Biometry
#Lab 1
# ---- question 1 ----

females <- c(3,2,1,5,4,3)
males <- c(2,5,3,4,1,2)

#sex ratio of first mother
females[1] / (females[1] + males[1])

#sex ratio for all mothers
(ratios <- females / (females + males))

#avg sex ratio
mean(ratios)

# ---- question 2 ----
getwd()
setwd('.')
rm(list = ls())
bmi <- read.csv("bmi.csv")
dim(bmi)
names(bmi)
mean(bmi$weight)
bmi$bmi <- bmi$weight / (bmi$height/100)^2

plot(bmi$weight,bmi$bmi, xlab="Weight", ylab="BMI")

# ---- question 3 ----
toxin <- read.csv("toxin.csv")
plot(toxin$Time,toxin$Concentration,xlab = "Time", ylab = "Concentration")
plot(toxin$Time,log(toxin$Concentration), xlab = "Time", ylab = "Log Concentration")

# ---- question 4 ----
lizards <- read.csv("lizards.csv")
maleweight <- lizards$Weight[lizards$Sex == "male"]
femaleweight <- lizards$Weight[lizards$Sex == "female"]
(meanmale <- mean(maleweight))
(meanfemale <-mean(femaleweight))

barplot(c(meanmale,meanfemale),names.arg = c("Male","Female"))

# ---- question 5 ----

lions <- read.csv("lions.csv")
maleage <- lions$Age[lions$Sex == "male"]
maleproportion <- lions$Black[lions$Sex == "male"]
femaleage <- lions$Age[lions$Sex == "female"]
femaleproportion <- lions$Black[lions$Sex == "female"]
plot(maleage, maleproportion, pch=16, col="black", xlab = "Age", ylab = "Proportion Black")
points(femaleage, femaleproportion, pch=16, col="red")
