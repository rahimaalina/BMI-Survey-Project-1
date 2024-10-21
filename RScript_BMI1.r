
###########################################################################
## Set the working directory

## In RStudio the working directory is easily set via the menu
## "Session -> Set Working Directory -> To Source File Location" 
## Note: In R only "/" is used for separating in paths 
## (i.e. no backslash).
setwd("~/DTU/3rd Semester/Statistics/Projects/BMI_P1_ST")


###########################################################################
## Read data into R

## Read data from bmi1_data.csv
D <- read.table("bmi1_data.csv", header = TRUE, sep = ";", as.is = TRUE)


###########################################################################
## Simple overview of the data

## Dimensions of D (number of rows and columns)
dim(D)
##  Column/variable names
names(D)
## The first rows/observations
head(D)
## The last rows/observations
tail(D)
## Selected summary statistics
summary(D)
## Another type of summary of the dataset
str(D)


###########################################################################
## Calculate BMI scores

## Calculate BMI scores and add new variable to D
D$bmi <- D$weight/(D$height/100)^2


###########################################################################
## Histogram (empirical density)

## Histogram describing the empirical density of the BMI scores
## (histogram of the BMI scores normalized to have an area of 1)
hist(D$bmi, xlab = "BMI", ylab = "Density", xlim = c(15, 40), ylim = c(0, 0.12),
     main = "Density Histogram of the BMI Scores", col = "deepskyblue", 
     border = "deepskyblue4", prob = TRUE)

## Summary statistics of BMI scores
mean(D$bmi)
median(D$bmi)
var(D$bmi)
sd(D$bmi)


###########################################################################
## Taking subsets of the data using 'subset'

## Divide data into two subsets according to gender
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)


###########################################################################
## Density histograms by gender

## Density histograms describing the empirical density
## of the BMI scores of women and men, respectively.
hist(Dfemale$bmi, xlab = "BMI (female)", ylab = "Density", xlim = c(15, 40), 
     ylim = c(0, 0.14), main = "Histogram of Female BMI Scores", 
     col = "coral", border = "coral4", prob = TRUE)
hist(Dmale$bmi, xlab = "BMI (male)", ylab = "Density", xlim = c(15, 40), 
     ylim = c(0, 0.15), main = "Histogram of Male BMI Scores", 
     col = "darkolivegreen2", border = "darkolivegreen4", prob = TRUE)

## Summary statistics of female BMI scores
mean(Dfemale$bmi)
median(Dfemale$bmi)
sd(Dfemale$bmi)

## Summary statistics of male BMI scores
mean(Dmale$bmi)
median(Dmale$bmi)
sd(Dmale$bmi)

###########################################################################
## Box plot

## Box plot of BMI scores by gender
boxplot(Dfemale$bmi, Dmale$bmi, names = c("Female", "Male"), 
        main = "Box Plot of BMI Scores by Gender", xlab = "Gender", ylab = "BMI", 
        col = c("coral", "darkolivegreen2"), border = c("coral4", "darkolivegreen4"))


###########################################################################
## Summary statistics for BMI

## Total number of observations
## (doesn't include missing values if there are any)
sum(!is.na(D$bmi))
## Sample mean (both genders combined)
mean(D$bmi, na.rm = TRUE)
## Sample variance (both genders combined)
var(D$bmi, na.rm = TRUE)
## Sample standard deviation (both genders combined)
sd(D$bmi, na.rm = TRUE)
## Lower quartile, median and upper quartile (both genders combined)
quantile(D$bmi, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

## Total number of observations (female)
## (doesn't include missing values if there are any)
sum(!is.na(Dfemale$bmi))
## Sample mean (female)
mean(Dfemale$bmi, na.rm = TRUE)
## Sample variance (female)
var(Dfemale$bmi, na.rm = TRUE)
## Sample standard deviation (female)
sd(Dfemale$bmi, na.rm = TRUE)
## Lower quartile, median and upper quartile (female)
quantile(Dfemale$bmi, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

## Total number of observations (male)
## (doesn't include missing values if there are any)
sum(!is.na(Dmale$bmi))
## Sample mean (male)
mean(Dmale$bmi, na.rm = TRUE)
## Sample variance (male)
var(Dmale$bmi, na.rm = TRUE)
## Sample standard deviation (male)
sd(Dmale$bmi, na.rm = TRUE)
## Lower quartile, median and upper quartile (male)
quantile(Dmale$bmi, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

## The argument 'na.rm=TRUE' ensures that the statistic is
## computed even in cases where there are missing values.


###########################################################################
## qq-plot for model validation

## New variable 'logbmi' with log-transformed BMI
D$logbmi <- log(D$bmi)

## qq-plot of log-transformed BMI
qqnorm(D$logbmi, lwd = 2, main = "Q-Q Plot BMI Scores Both Genders")
qqline(D$logbmi, lwd = 2, col = "deepskyblue2")

## Mean and standard deviation of log-transformed BMI scores
mean(D$logbmi)
sd(D$logbmi)

###########################################################################
## 95% Confidence Interval for mean log-transformed data (both genders combined)
qt(0.975, 144)

3.22 + 1.977*(0.15/sqrt(145))
3.22 - 1.977*(0.15/sqrt(145))

## 95% CI for median BMI scores (both genders)
exp(3.22 + 1.977*(0.15/sqrt(145)))
exp(3.22 - 1.977*(0.15/sqrt(145)))

###########################################################################
## One-sample t-test

## Computing t-statistic
tobs <- (3.22 - log(25))/(0.15/sqrt(145))

## Computing the p-value
2 * (1 - pt(abs(tobs), df = 144))

## Testing hypothesis mu=log(25) for log-transformed BMI
t.test(D$logbmi, mu=log(25))

###########################################################################
## qq-plot of log-transformed by gender

## New variable with log-transformed BMI by gender
Dfemale$logbmi <- log(Dfemale$bmi)
Dmale$logbmi <- log(Dmale$bmi)

## qq-plot of log-transformed female BMI scores
qqnorm(Dfemale$logbmi, lwd = 2, main = "Q-Q Plot Female log BMI")
qqline(Dfemale$logbmi, lwd = 2, col = "coral3")

## Mean and standard deviation of log-transformed female BMI scores
mean(Dfemale$logbmi)
s_f <- sd(Dfemale$logbmi)
s_f

## qq-plot of log-transformed male BMI scores
qqnorm(Dmale$logbmi, lwd = 2, main = "Normal Q-Q Plot Male log BMI")
qqline(Dmale$logbmi, lwd = 2, col = "darkolivegreen4")

## Mean and standard deviation of log-transformed male BMI scores
mean(Dmale$logbmi)
s_m <- sd(Dmale$logbmi)
s_m


###########################################################################
## CI's for the mean and median

## 95% Confidence Interval for mean log-transformed data (female)
qt(0.975, 72)

3.17 + 1.993*(0.16/sqrt(72))
3.17 - 1.993*(0.16/sqrt(72))

## 95% CI for median BMI scores (female)
exp(3.17 + 1.993*(0.16/sqrt(72)))
exp(3.17 - 1.993*(0.16/sqrt(72)))

## 95% Confidence Interval for mean log-transformed data (male)
qt(0.975, 73)

3.26 + 1.993*(0.12/sqrt(73))
3.26 - 1.993*(0.12/sqrt(73))

## 95% CI for median BMI scores (male)
exp(3.26 + 1.993*(0.12/sqrt(73)))
exp(3.26 - 1.993*(0.12/sqrt(73)))


## Compute CI for mean log-BMI score of a woman
CI_f <- t.test(Dfemale$logbmi, conf.level=0.95)$conf.int
CI_f

## "Back-transform" to get a CI for median BMI score of a woman
exp(CI_f)

## Compute CI for mean log-BMI score of a man
CI_m <- t.test(Dmale$logbmi, conf.level=0.95)$conf.int
CI_m

## "Back-transform" to get a CI for median BMI score of a man
exp(CI_m)


###########################################################################
## Welch t-test for comparing two (independent) samples

## Computing tobs
tobs_2 <- (3.17 - 3.26)/sqrt((0.16^2/72) + (0.12^2/73))
tobs_2

## Computing the number of degrees v
v <- (((s_f^2/72) + (s_m^2/73))^2) / ((((s_f^2 / 72)^2) / 71) + (((s_m^2 / 73)^2) / 72))
v

## Computing the p-value 
2*(1 - pt(abs(tobs_2), df = v))

## Comparison of mean logBMI for women and men
t.test(Dfemale$logbmi, Dmale$logbmi)


###########################################################################
## Computing correlations

## Correlation between BMI and weight
(cov(D$bmi, D$weight))/(sd(D$bmi)*sd(D$weight))
cor(D$bmi, D$weight)

## Scatterplot of correlation between BMI and weight
plot(D$bmi, D$weight, xlab = "BMI [kg/m^2]", ylab = "Weight [kg]", 
     main = "Correlation between BMI and Weight", col = "deepskyblue2", lwd = 2)

## Correlation between BMI and fast food
(cov(D$bmi, D$fastfood))/(sd(D$bmi)*sd(D$fastfood))
cor(D$bmi, D$fastfood)

## Scatterplot of correlation between BMI and fast food
plot(D$bmi, D$fastfood, xlab = "BMI [kg/m^2]", ylab = "Fast Food [days/year]", 
     main = "Correlation between BMI and Fast Food Intake", col = "deepskyblue2", lwd = 2)

## Correlation between weight and fast food
(cov(D$weight, D$fastfood))/(sd(D$weight)*sd(D$fastfood))
cor(D$weight, D$fastfood)

## Scatterplot of correlation between weight and fast food
plot(D$weight, D$fastfood, xlab = "weight [kg]", ylab = "Fast Food [days/year]", 
     main = "Correlation between Weight and Fast Food Intake", col = "deepskyblue2", lwd = 2)

## Computing correlations between selected variables
cor(D[,c("weight","fastfood","bmi")], use="pairwise.complete.obs")