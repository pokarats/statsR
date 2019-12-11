### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates) name, matriculation number. 
## Name: Noon Pokaratsiri Goldstein, Pauline Sander, Axel Allen
## Matriculation number: 2581469, 2581310, 2579024

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################



#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)
library(dplyr)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

# a) For the further reference please use ?amis. 
# It may take some time to understand the dataset. 
?amis

# b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
# Feel free to make some plots and calculate some statistics in order to understand 
# the data.
data <- amis
glimpse(data)
head(data)
# c) All our columns have numeric type. Convert the categorial columns to factors.
cols <- c("period", "warning", "pair")
data[cols] <- lapply(data[cols], factor)
glimpse(data)

# d) Plot boxplots for the distribution of `speed` for each of the `period` values 
# (before, immediately after and after some time). Build 2 plots (each containing 3 
# boxplots) side by side depending on the `warning` variable.
# (For all plots here and below please use ggplot)
library(gridExtra)
data_w1 <- filter(data, warning==1)
data_w2 <- filter(data, warning==2)
ggp1 <- ggplot(data_w1, aes(period, speed)) + geom_boxplot() + facet_wrap(~period)
ggp2 <- ggplot(data_w2, aes(period, speed)) + geom_boxplot() + facet_wrap(~period)
grid.arrange(ggp1, ggp2, ncol = 2)  

# e) What can you conclude looking at the plots? What can you say about people's 
# behaviour in different periods: before, immediately after and after some time?
# From the boxplots it seems that the average speed is slight lower, but there are just as many
# individuals who drive excessively faster (outliers). After some time, it seems the average speed
# is back to being as high if not higher than when there was no sign.


# f) What are your ideas about why the data with warning==2 (sites where no sign was 
# erected) was collected?

# In comparison to the site where a warning sign was erected, the average speed doesn't seem to vary
# that much when no sign is erected at all, although it seems that there are more instances of people 
# going excessively faster.

#######################
### Exercise 2: 1-way ANOVA
#######################

# a) First let's create a new data frame which will be used for all exercise 2.
# For the 1-way ANOVA we will be working with a subset of `amis` using only the 
# data for sites where warning signs were erected, which corresponds to warning==1. 
# Therefore first subset your data to filter out warning==2 and then apply cast() 
# to average "speed" over each "pair" and "period". 
# Assign this new data frame to the variable casted_data.
data_w1 <- filter(data, warning==1)
mdata <- melt(data_w1, id.vars = c("pair", "period"),
                 measure.vars = c("speed"))
casted_data <- dcast(mdata, pair + period ~ variable, mean, na.rm = T)

# b) Build boxplots of the average speed depending on "period".
ggplot(casted_data, aes(period, speed, group_by(period))) + geom_boxplot() # + facet_wrap(~period)

# c) Looking at the boxplots, is there a difference between the periods?
# The mean speed is much lower in period 2 than in 1 and 3. There also seems to be quite a difference
# between the mean speed in period 1 and 3.

# Now, let's check the ANOVA assumptions and whether they are violated or not 
# and why.

# d) Independence assumption
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)
duplicated(casted_data)
# no repeated measures

# e) Normality of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)
shapiro.test(casted_data$speed)
# The Shapiro-Wilk test's null hypothesis is that the sample distribution is normal.
# So, the p-value of 0.3494, which is higher than alpha == 0.05, suggests that the Speed variable has a normal
# distribution
?shapiro.test
qqnorm(casted_data$speed)

# to verify that our residuals ar normal, we can use the Shapiro Wilk Test and also plot the QQ-plot of the
# residuals
mod <- lm(speed ~ period, data = casted_data)
summary(mod)
mod_augmented <- mod %>% augment()
print(mod_augmented)
shapiro.test(mod_augmented$.resid)
qqnorm(mod_augmented$.resid)
qqline(mod_augmented$.resid)

# based on the Shapiro - Wilk test p-value of 0.1662, which is higher than alpha of 0.05, we can conclude
# that the residuals follow a normal distribution. Our QQ plot also confirms this as the points do somewhat
# follow a straight line.

# f) Homogeneity of variance of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)
plot(mod)
# based on the Residuals vs Fitted values plot of the model, it seems that the residuals vary somewhat the same way
# across all values

# Checking with the Breusch - Pagan test; since our residuals follow the normal distribution, we should be
# able to use this test to see if there's heteroskasdicity in the residuals
install.packages('lmtest')
library(lmtest)
bptest(mod)
# The BP test null hypothesis is homoskedasticity i.e. the residuals variances are all equal
# BP p-value of 0.6996 means that we cannot reject the null hypothesis, so that means our residuals
# follow the homogeneity assumption. Hence, we should be able to do regression on this data set.

# g) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
# speed depending on the period, report p-value and interpret the result in details.
summary(aov(speed ~ period, data = casted_data))
# The p-value from 1-way ANOVA of 0.382 suggests that we cannot reject the null hypothesis.
# Thus, we conclude that the variability in speed is not dependent on the period of time a sign has been
# put up on the road.

# h) Please do pairwise t-tests of the same variables as in g) using pairwise.t.test().
?t.test
t.test(speed ~ period, data = casted_data, subset = period %in% c(1,2), paired = T)
t.test(speed ~ period, data = casted_data, subset = period %in% c(1,3), paired = T)
t.test(speed ~ period, data = casted_data, subset = period %in% c(2,3), paired = T)

# i) Report the pairwise p-values and interpret the result in detail.
# since there are 3 levels in our period predictor variable, we have to subset the period variable into groups of
# 2 levels so we have 3 p-values based on running the t-tests on period in 1,2, in 1,3, and in 2,3.
# p-values for periods 1,2 == 0.3332; periods 1,3 == 0.004334, periods 2,3 0.04114

# j) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
# Does the result change?


#######################
### Exercise 3: 2-way ANOVA
#######################
# a) Now we want to analyze the influence of 2 categorial variables 
# (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1).
# First, we need to average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2.

# b) Calculate the mean for each of the 6 possible pairs of `period` and `warning`.


# c) Do you think there is a significant difference between some of the groups?


# d) Now apply the 2-way ANOVA: please use the function aov() on the speed depending 
# on the period and warning.
# Report the p-value and interpret the result in detail.


# e) What do you conclude about the behaviour of drivers based on the 2-way ANOVA?



