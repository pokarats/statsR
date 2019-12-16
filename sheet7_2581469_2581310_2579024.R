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
library(reshape2)
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

# From the first boxplot it seems that the average speed is slightly lower right after the sign has been put
# up, but there are just as many individuals who drive excessively faster (outliers). 
# After some time, it seems the average speed is back to being as high if not higher than when there was no sign, 
# as shown in the first boxplot. In comparison to the site where a warning sign was erected, the average speed doesn't seem to vary
# that much when no sign is erected at all, although it seems that there are more instances of people 
# going excessively faster.

# f) What are your ideas about why the data with warning==2 (sites where no sign was 
# erected) was collected?

# This is so that we have a control group for the speeds at similar locations.

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
# between the mean speed in period 1 and 3. In period 2, there are more extreme speeds that are above the average 
# speed while in period 1 and 3, there are more speed variabilities that lower than the average speed.
 
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

data_lm = lm(speed ~ period, casted_data)
data_stdres <- rstandard(data_lm)
shapiro.test(data_stdres)

# based on the Shapiro - Wilk test p-value of 0.1662, which is higher than alpha of 0.05, we can conclude
# that the residuals follow a normal distribution. Our QQ plot also confirms this as the points do somewhat
# follow a straight line.

qqnorm(data_stdres)
qqline(data_stdres)
summary(data_lm)
# f) Homogeneity of variance of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)
library(car)
leveneTest(speed ~ period, data=casted_data)
# g) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
# speed depending on the period, report p-value and interpret the result in details.
aov_data <- aov(speed ~ period, data=casted_data)
summary(aov_data)
# The p-value from 1-way ANOVA of 0.382 suggests that we cannot reject the null hypothesis.
# Thus, we conclude that the variability in speed is not dependent on the period of time the sign has been
# put up on the road.

# h) Please do pairwise t-tests of the same variables as in g) using pairwise.t.test().
pairwise.t.test(casted_data$speed, casted_data$period)

# i) Report the pairwise p-values and interpret the result in detail.

# The p-values for the difference in speeds between period 1 & 2 and between period 1 & 3 are both 0.81. 
# The p-value for the difference in speeds between period 2 & 3 is 0.51.
# All the 3 p-values are greater than the 0.05 significant level, which suggests that any differences in speeds
# we may have observed in our data between the periods are simply due to chance. i.e. there's really no difference
# in speeds.

# j) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
# Does the result change?
pairwise.t.test(casted_data$speed, casted_data$period, p.adjust.method = "bonferroni")

# With the adjustment, the p-values for the differences in speeds between period 1&2 and between period 1&3
# are now 1.00, which doesn't change our conclusion regarding accepting the null hypothesis.

#######################
### Exercise 3: 2-way ANOVA
#######################
# a) Now we want to analyze the influence of 2 categorial variables 
# (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1).
# First, we need to average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2.
mdata <- melt(data, id.vars = c("pair", "warning", "period"),
              measure.vars = c("speed"))
casted_data2 <- dcast(mdata, pair + warning + period ~ variable, mean, na.rm = T)
# b) Calculate the mean for each of the 6 possible pairs of `period` and `warning`.
means <- with(casted_data2, aggregate(speed, by=list(pair = pair, warning=warning), FUN=mean, na.rm=TRUE))
colnames(means)[3] <- "speed"

# c) Do you think there is a significant difference between some of the groups?
ggplot(means, aes(pair, speed, color=warning)) + geom_point()
#There is a very significant difference in group 7

# d) Now apply the 2-way ANOVA: please use the function aov() on the speed depending 
# on the period and warning.
aov(speed ~ period*warning, data=casted_data2)
summary(aov(speed ~ period*warning, data=casted_data2))
# Report the p-value and interpret the result in detail.

# The only p-value that's less than alpha = 0.05 is the p-value from the F-ratio from speed ~ warning, which is
# 0.00488. This suggests that the variability in speed can be explained by having a warning sign.

# Since the p-value for the period factor is > 0.05 (0.335), we can conclue that the variability in speed is
# not dependent on the period of time the sign has been put up.

# The p-value of 0.69975 between 'period' and 'warning' suggests that there's no interactional effects between
# the two independent factors.

# e) What do you conclude about the behaviour of drivers based on the 2-way ANOVA?

# Based on the 2-way ANOVA, we can conclude that the presence of a warning sign has a statistically significant
# effect on drivers' speed. Based on the speed grouped by warning plot, we may conclude more specifically that
# drivers are likely to drive faster when there's no warning sign.


