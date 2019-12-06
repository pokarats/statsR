### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 9. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.


## Please write below your (and your teammates') name, matriculation number. 
## Name: Noon Pokaratsiri Goldstein, Pauline Sander, Axel Allen
## Matriculation number: 2581469, 2581310, 2579024

## Change the name of the file by adding your matriculation numbers
## (sheet06_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################


library(reshape)
library(languageR)

#######################
### Exercise 1: Correlation
#######################

# a) Get some data - access the ratings data set in languageR and name it "data".
# The data set contains subjective frequency ratings and their length averaged over 
# subjects, for 81 concrete English nouns.
data <- ratings

# b) Take a look at the data frame.
glimpse(data)
summary(data)
# c) Let's say you're interested in whether there is a linear relationship between 
# the word frequency of the 81 nouns and their length.
# Take a look at the relationship between the frequency and word length data by 
# means of a scatterplot (use the ggplot library for this).
library(ggplot2)
ggplot(data, aes(x = Length, y = Frequency)) +
  geom_point(size = 2, shape = 21)
# d) Judging from the graphs, do you think that word frequency and word length are 
# in any way correlated with one another?
# it looks like there may be an inverse relatinoship between the two variables.

# e) Compute the Pearson correlation coefficient for the two variables by means 
# of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variables 
# divided by the product of their respective variance. 
# It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
cor(data$Length, data$Frequency, use = 'pairwise.complete.obs')
# since we don't have missing values in the data set, not specifying the use = argument also 
# resulted in the same thing
cor(data$Length, data$Frequency)
# f) Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?
# the negative number means a negative correlation; i.e. as the predictor variable increases/decreases by one
# unit point, the response variable decreases/increases by a certain factor in the opposite direction 
# based on the scale of -1 to 1, 0.43 would suggest a medium correlational effect

# g) Note that we have a large number of tied ranks in word length data 
# (since there are multiple words with the length of e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to 
# Kendall's tau instead of the Pearson correlation coefficient (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?
cor(data$Length, data$Frequency, method = 'kendall')
# Kendall's coefficient doesn't assume that you sample follows a normal distribution.
# It's also able to analyze data with tied values i.e. multiple y-values for same x-values.
# Kendall's coefficient also describes how dependent the two variables are on each other

# h) What about significance? Use the more user-friendly cor.test()!
# Take a look at the output and describe what's in there.
# What do you conclude?
cor.test(data$Length, data$Frequency, method = 'kendall')
?cor.test
#cor.test also includes z score and p-value in addition to the rank correlation coefficient
# The p-value of 8.907*10^-5 suggests that it very unlikely that the null hypothesis is true. So, we can reject
# the null hypothesis and conclude that the relationship between the 2 variables is due to chance.

# i) Finally, also calculate Spearman's rank correlation for the same data.
cor.test(data$Length, data$Frequency, method = 'spearman')

#######################
### Exercise 2: Regression
#######################

# a) Fit a linear regression model to the data frame "data" from exercise 1 
# for the variables Frequency (outcome variable) and Length (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
lm_data <- lm(Frequency ~ Length, data = data, na.action = na.omit)
print(lm_data)
# b) How do you interpret the output? Is the relationship between the two variables 
# positive or negative?
# Plot the data points and the regression line.

# The y-intercept is the expected Frequency for a word of Length 0 (6.5015)
# The slope of -0.2943 suggests a negative correlation between Length and Frequency; as word length
# increases by 1 character, the word frequency becomes more sparse.
ggplot(data, aes(Length, Frequency)) + 
  geom_point(shape = 21) + geom_abline(slope = -0.2943, intercept = 6.5015)

ggplot(data, aes(Length, Frequency)) + 
  geom_point(shape = 21) + geom_smooth(method = 'lm', se = F)

# c) Run the plotting command again and have R display the actual words that belong 
# to each point. 
# (Don't worry about readability of overlapping words.)
ggplot(data, aes(Length, Frequency)) + 
  geom_point(color='blue', size = 1) + 
  geom_text(aes(label = Word, hjust = 0, vjust = 0)) +
  geom_smooth(method = 'lm', se = F)

#######################
### Exercise 3: Regression
#######################


# a) Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv and store it in a variable.
# You can download this data frame from the material of week 6: t-test and friends. 
library(reshape2)
library(tidyr)
library(dplyr)
data_dc <- read.csv('digsym_clean.csv')
glimpse(data_dc)

# b) Suppose you want to predict reaction times in the digit symbol task by 
# people's age.
# Fit a linear regression model to the data frame for the variables 
# correct_RT_2.5sd (outcome variable) and Age (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
# But first we need to cast the data to compute an RT mean (use correct_RT_2.5sd) 
# for each subject, so that we have only one Age observation per Subject.
# Store the result in a new dataframe called "cast".
# In case you're wondering why we still have to do this - like the t-test, 
# linear regression assumes independence of observations.
# In other words, one row should correspond to one subject or item only.

?cast
mdata_dc <- melt(data_dc, id.vars = c("Subject", "Age"),
              measure.vars = c("correct_RT_2.5sd"))
cdata_dc <- dcast(mdata_dc, Age + Subject ~ variable, mean, na.rm = T)
# c) Now fit the regression model.
lm_cdata_dc <- lm(correct_RT_2.5sd ~ Age, data = cdata_dc, na.action = na.omit)

# d) Let's go over the output - what's in there?
# How do you interpret the output?
print(lm_cdata_dc)
# interpretation: the positive slope (21.22) suggests a positive correlation between the 2 variables.
# as Age increases, response time also increases by a factor of 21.22.
# The y-intercept is the expected level of response time for Subjects at Age 0 (if that were to exist, that is.).

# e) Plot the data points and the regression line. 
ggplot(cdata_dc, aes(Age, correct_RT_2.5sd)) +
  geom_point(color = 'blue', size = 1) +
  geom_smooth(method = 'lm', se = F, color = 'red')

# f) Plot a histogram and qq-plot of the residuals. 
# Does their distribution look like a normal distribution?
library(broom)
residuals <- resid(lm_cdata_dc)
qqnorm(residuals)
install.packages('lindia')
library(lindia)
gg_reshist(lm_cdata_dc, bins = 15)

# another way just using straightforward ggplot
mod <- lm_cdata_dc %>% augment()
ggplot(mod, aes(.resid)) + geom_histogram(bins = 15)

# based on the qqnorm plot and the histogram, the residuals seem to more or less have a normal distribution
# except for an outlier as shown in the histogram plot
# g) Plot Cook's distance for the regression model from c) which estimates the 
# residuals (i.e. distance between the actual values and the  predicted value on 
# the regression line) for individual data points in the model.
gg_cooksd(lm_cdata_dc) # with lindia library

# with just ggplot and augment()
ggplot(mod, aes(seq_along(.cooksd), .cooksd)) + 
  geom_pointrange(aes(ymin = 0, ymax = mod$.cooksd), size = 0.2)
  xlab('Observation Number')+ ylab('Cook\'s distance') 
  
# h) Judging from the plot in g) it actually looks like we have 1 influential 
# observation in there that has potential to distort (and pull up) our regression 
# line.
# The last observation (row 37) in cast has a very high Cook's distance 
# (greater than 0.6).
# In other words, the entire regression function would change by more than 
# 0.6 when this particular case would be deleted.
# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to 
# each point.
ggplot(mod, aes(seq_along(.cooksd), .cooksd)) + 
  geom_pointrange(aes(ymin = 0, ymax = mod$.cooksd), size = 0.2) +
  geom_text(aes(label = cdata_dc$Age, hjust = 0, vjust = 0)) +
  xlab('Observation Number')+ ylab('Cook\'s distance') 

# The predictor variable (Age) at observation 37 is much higher than the other subjects.

# i) Make a subset of "cast" by excluding the influential subject and name it cast2.
cast2 <- cdata_dc %>% filter(Age < 45)
# j) Fit the model from c) again, using cast2, and take a good look at the output.
lm_cast2 <- lm(correct_RT_2.5sd ~ Age, data = cast2, na.action = na.omit)
print(lm_cast2)



# k) What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?
# The slope is roughly half of the previous model that included the outlier data point.
# This suggests that perhaps the association between age and RTs is not as strong as previously thought or that
# the association between age and RTs are not as strong for population groups younger than 45.

# l) Plot the regression line again - notice the difference in slope in 
# comparison to our earlier model fit?
ggplot(cast2, aes(Age, correct_RT_2.5sd)) +
  geom_point(color = 'blue', size = 1) +
  geom_smooth(method = 'lm', se = F, color = 'red')

# m) Display the two plots side by side to better see what's going on.
library(gridExtra)
no_outlier_plot <- ggplot(cast2, aes(Age, correct_RT_2.5sd)) +
  geom_point(color = 'blue', size = 1) +
  geom_smooth(method = 'lm', se = F, color = 'red')
outlier_plot <- ggplot(cdata_dc, aes(Age, correct_RT_2.5sd)) +
  geom_point(color = 'blue', size = 1) +
  geom_smooth(method = 'lm', se = F, color = 'red')
grid.arrange(no_outlier_plot, outlier_plot, ncol=2)

# n) Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Take a look at the Navarro book (Chapter on regression) if you have trouble 
# doing this.
?mutate
mod_cast2 <- lm_cast2 %>% augment()
summary(mod_cast2)
# R^2 = 1 - Var(e)/Var(y)

# without outlier
mod_cast2 %>% 
  summarize(var_y = var(correct_RT_2.5sd), var_e = var(.resid)) %>%
  mutate(R_squared = 1 - (var_e/var_y))

# with outlier
mod %>% 
  summarize(var_y = var(correct_RT_2.5sd), var_e = var(.resid)) %>%
  mutate(R_squared = 1 - (var_e/var_y))
# o) How do you interpret R Squared?
# R Squared value measures how well the model (regression line) fits the actual observed data.
# The R-squared value of 0.0349, when outlier is removed from the data, suggests that only 3.49% of the variability
# in the correct response time can be explained by age.
# When outlier data is included, this changes the R-squared value to 0.178, which suggests that as much as 17.8% of the
# variability in the correct response time can be explained by age -- quite a noticeable difference!
