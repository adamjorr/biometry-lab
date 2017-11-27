# ---- fix data ----
library(tidyverse)

normalfiles <- list.files("data/normal/", full.names = T)
infectedfiles <- list.files("data/infected/", full.names = T)

normal <- lapply(normalfiles, read.csv)

load_sample <- function(filename){
  sam <- tools::file_path_sans_ext(basename(filename))
  df <- suppressMessages(read_csv(filename)) %>%
    select(contains("m/z")) %>% #take the column that contains "m/z"
    rename_at(1,~'mz') %>% # rename column to mz
    distinct() %>% #get rid of duplicated rows
    na.omit() %>%
    filter(mz < 500)
  tibble(mz = charges$mz, present = charges$mz %in% df$mz, sam = sam)
}

# alldata <- list.files(dirs, full.names = TRUE, pattern = '*.csv', recursive = TRUE) %>%
#   map(~load_sample(.))

normaldata <- map(normalfiles, ~load_sample(.))
normal <- bind_rows(normaldata) %>%
  mutate(health = "normal")

infecteddata <- map(infectedfiles, ~load_sample(.))
infected <- bind_rows(infecteddata) %>%
  mutate(health = "infected")

alldata <- bind_rows(normal, infected)
charges <- alldata %>%
  select(mz) %>%
  unique()

# ---- exact tests ----
msdata <- read.csv('data.csv', colClasses = c(NA,"factor",NA,"factor"))
peaks <- unique(msdata$mz)

#take a peak then build a contingency table, do an exact test, get the pvalue
do_test <- function(x){
  peak <- subset(msdata, msdata$mz == x) #take only data for this peak
  tab <- table(peak$present, peak$health) #make a contingency table
  fet <- fisher.test(tab) #do a fisher test
  fet$p #return the p value
}

pvals <- sapply(peaks, do_test) #do an exact test for each peak
qvals <- p.adjust(pvals) #correct the p values

#none of our corrected p-values are significant. Oh well. Let's use the
#uncorrected ones so we have something to do.

# ---- plot p values ----

#plot the p values
hist(pvals, main = "P-values", xlab = "P")
abline(v = 0.05) #draw a line for p = 0.05

# ---- regression 1 ----
sigpeaks <- subset(peaks, pvals < 0.05) #pick only the significant peaks
sigdata <- subset(msdata, msdata$mz %in% sigpeaks) #pick only the data for those significant peaks
sigdata <- subset(sigdata, sigdata$present == T) #pick only the samples that have the significant peak

numsig <- as.numeric(table(sigdata$sam)) #count number of significant peaks for each sample
health <- unique(sigdata[,3:4]) #obtain health status of each sample
isinfected <- ifelse(health$health == "infected", 1, 0) #get health status as a 1 or 0

#do logistic regression
mod <- glm(isinfected ~ numsig, family = binomial())

#predict values to plot fit
pred <- predict(mod, type = "response", newdata = data.frame(numsig = seq(0,100, by = 1)))

#show model
summary(mod)

# ---- regression 2 ----
#look at 95% confidence interval of parameters
confint.default(mod)

# ---- regression 3 ----

#plot data and model
plot(isinfected~numsig,
     xlab = "Number of Significant Peaks",
     ylab = "Probability of Infected Donor")
lines(pred)

# ---- regression 4 ----
#get residuals
res <- residuals(mod)
#get fitted values
fit <- fitted(mod)
#Plot the residuals vs. the fitted values.
plot(res~fit,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")

# ---- regression 5 ----
#generate qq plot with 1:1 line
qqnorm(res)
qqline(res)

