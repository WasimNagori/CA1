# Load the assigned dataset
sleep_data <- read.csv("sleep(1).csv")

# Reviewing structure and statistical summary of the dataset
str(sleep_data)
summary(sleep_data)

# Preparing the data
names(sleep_data)[names(sleep_data) == "sr.1"] <- "sh"
sleep_data$stress <- factor(sleep_data$sl, labels = c("low", "medium low", "medium", "medium high", "high"))

# Q1) Snoring rate v/s Stress level
pairs(sleep_data, labels = colnames(sleep_data), main = "sleep dataset correlation plot")

# Plot Histogram
hist(sleep_data$sr, col="royalblue",xlab = "Snoring rate", ylab = "Frequency",
     main='Frequency chart of data distribution of Snoring rate')

library("lattice")
histogram(~sr | stress, 
          data = sleep_data, 
          main = "Distribution of snoring ratebased on different levels of the stress variable", 
          xlab = "Snoring Rate", 
          ylab = "Stress Level")

# Q-Q plot can help to determine normality
qqnorm(sleep_data$sr, col = "skyblue")
qqline(sr, col = "darkgreen")

opar <- par(no.readonly = TRUE)
# arrange plots in 1 row and 5 columns
par(mfrow = c(1, 5))

with(sleep_data, {
  qqnorm(sr[stress == "low"], 
         main = " Snoring rate at low stress level")
  qqline(sr[stress == "low"], col = "red")
})

with(sleep_data, {
  qqnorm(sr[stress == "medium low"], 
         main = " Snoring rate at medium low stress level")
  qqline(sr[stress == "medium low"], col = "red")
})

with(sleep_data, {
  qqnorm(sr[stress == "medium"], 
         main = " Snoring rate at medium stress level")
  qqline(sr[stress == "medium"], col = "red")
})

with(sleep_data, {
  qqnorm(sr[stress == "medium high"], 
         main = " Snoring rate at medium high stress level")
  qqline(sr[stress == "medium high"], col = "red")
})

with(sleep_data, {
  qqnorm(sr[stress == "high"], 
         main = " Snoring rate at high stress level")
  qqline(sr[stress == "high"], col = "red")
})
par(opar)

# Statistical test to check normality
normality_test <- shapiro.test(sleep_data$sr)
normality_test$p.value # Not normalised

with(sleep_data, tapply(sr, stress, shapiro.test)) # Not normalised

# Format of the test is Kruskal-Wallis test
# Run Kruskal-Wallis test
kruskal.test(sr ~ sl, data = sleep_data)
# h1 is true


# Q2) Respiration rate (rr) v/s Body temperature (t)
hist(sleep_data$rr, col="#f6f805",xlab = "Respiration rate", ylab = "Frequency",
     main='Frequency chart of data distribution of Respiration rate')

hist(sleep_data$t, col="seagreen",xlab = "Body Temperature", ylab = "Frequency",
     main='Frequency chart of data distribution of Body Temperature')

# Q-Q plot can help to determine normality
qqnorm(sleep_data$rr, col = "gold", main = "Q-Q plot of Respiration Rate")
# this line represents normal distribution
qqline(sleep_data$rr, col = "red")

qqnorm(sleep_data$t, col = "purple", main = "Q-Q plot of Body Temperature")
qqline(sleep_data$t, col = "black")

# Statistical test to check normality
normality_test <- shapiro.test(sleep_data$rr)
normality_test$p.value # not normally distributed

normality_test <- shapiro.test(sleep_data$t)
normality_test$p.value # not normally distributed

# Format of the test is Spearmans Correlation Coefficient
res <- cor.test(sleep_data$rr, sleep_data$t,
                method = "spearman")
res
# h1 is true

# Q3) Sleeping Hours (sh) v/s Limb Movement (lm)

hist(sleep_data$sh, col="beige", xlab = "Sleeping hours", ylab = "Frequency",
     main='Frequency chart of data distribution of Sleeping Hours')

hist(sleep_data$lm, col="pink", xlab = "Limb movement", ylab = "Frequency",
     main='Frequency chart of data distribution of Limb Movement')

# Q-Q plot can help to determine normality
qqnorm(sleep_data$sh, col = "grey", main = "Q-Q plot of Sleeping Hours")
# this line represents normal distribution
qqline(sleep_data$sh, col = "black")

qqnorm(sleep_data$lm, col = "maroon", main = "Q-Q plot of Limb Movement")
qqline(sleep_data$lm, col = "royalblue")

# Statistical test to check normality
normality_test <- shapiro.test(sleep_data$sh)
normality_test$p.value # not normally distributed

normality_test <- shapiro.test(sleep_data$lm)
normality_test$p.value # not normally distributed

# Format of the test is Spearmans Correlation Coefficient
attach(sleep_data)
res <- cor.test(sleep_data$sh, sleep_data$lm,
                method = "spearman")
res
# h1 is true

# Q4) Heart rate (hr) v/s Stress level (sl)

# Histogram Plot
hist(sleep_data$hr, col="red", xlab = "Heart Rate", ylab = "Frequency",
     main='Frequency chart of data distribution of Heart Rate')

library("lattice")
histogram(~hr | stress, 
          data = sleep_data, 
          main = "Distribution of Heart rate based on different levels of the stress variable", 
          xlab = "Heart Rate", 
          ylab = "Stress Level",
          col = "blue")


# Q-Q plot can help to determine normality
qqnorm(sleep_data$hr, col = "navyblue", main = "Q-Q plot of Heart Rate")
qqline(hr, col = "red")

opar <- par(no.readonly = TRUE)
# arrange plots in 1 row and 5 columns
par(mfrow = c(1, 5))
with(sleep_data, {
  qqnorm(hr[stress == "low"], 
         main = " Heart rate at low stress level")
  qqline(hr[stress == "low"], col = "red")
})

with(sleep_data, {
  qqnorm(hr[stress == "medium low"], 
         main = " Heart rate at medium low stress level")
  qqline(hr[stress == "medium low"], col = "red")
})

with(sleep_data, {
  qqnorm(hr[stress == "medium"], 
         main = "Heart rate at medium stress level")
  qqline(hr[stress == "medium"], col = "red")
})

with(sleep_data, {
  qqnorm(hr[stress == "medium high"], 
         main = "Heart rate at medium high stress level")
  qqline(hr[stress == "medium high"], col = "red")
})

with(sleep_data, {
  qqnorm(hr[stress == "high"], 
         main = " Heart rate at high stress level")
  qqline(hr[stress == "high"], col = "red")
})
par(opar)


# Statistical test to check normality
normality_test <- shapiro.test(sleep_data$hr)
normality_test$p.value # Not normalised

with(sleep_data, tapply(hr, stress, shapiro.test)) # Not normalised

# Format of the test is Kruskal-Wallis test
# Run Kruskal-Wallis test
kruskal.test(hr ~ sl, data = sleep_data)
# h1 is true

# Q5) Blood oxygen (bo) v/s Rapid eye movement (rem)
# Plot histogram
hist(sleep_data$bo, col="gold", xlab = "Blood oxygen", ylab = "Frequency",
     main='Frequency chart of data distribution of Blood oxygen')

hist(sleep_data$rem, col="cyan", xlab = "Rapid Eye Movement", ylab = "Frequency",
     main='Frequency chart of data distribution of Rapid Eye Movement')

# Q-Q plot can help to determine normality
qqnorm(sleep_data$bo, col = "red", main = "Q-Q plot of Blood Oxygen")
# this line represents normal distribution
qqline(sleep_data$bo, col = "yellow")

qqnorm(sleep_data$rem, col = "steelblue", main = "Q-Q plot of Rapid Eye Movement")
qqline(sleep_data$rem, col = "black")

# Statistical test to check normality
normality_test <- shapiro.test(sleep_data$bo)
normality_test$p.value # not normally distributed

normality_test <- shapiro.test(sleep_data$rem)
normality_test$p.value # not normally distributed

# Format of the test is Spearmans Correlation Coefficient
res <- cor.test(sleep_data$bo, sleep_data$rem,
                method = "spearman")
res
# h1 is true