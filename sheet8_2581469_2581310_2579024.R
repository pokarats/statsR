### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 6. Write the code below the questions. 
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

###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########


# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1 = high school education, 0 = no high school degree.

data <- read.delim('kidiq.txt', sep = " ")
summary(data)

# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(data = data, aes(mom_iq, kid_score)) +
  ggtitle('Plot of kid_score and mom_iq') +
  geom_point(shape = 21) +
  geom_smooth(method = 'lm', se = T)
  
# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.

model <- lm(kid_score ~ mom_hs, data = data)
summary(model)

# based on the F-statistic p-value of 5.957 * 10^-7 at alpha = 0.05, the variability in kid_score can be said
# to be influenced by mom_hs. The slope of 11.771 on the regression line suggests that for every unit point
# increase in mom_hs, kid_score also increases by the factor of 11.771.
# Since mom_hs is either 0 for no high school education or 1 for having high school education, the y-intercept of 77.548
# suggests that kid_score whose mom doesn't have high school education is estimated to be 77.548 and 11.771 IQ points
# higher in those whose moms have high school education.

# The multiple R-squared of 0.05613 suggest that only 5.61% of kid_score variability is explained by mom_hs.
# Although the relationship between kid_score and mom_hs is statistically significant, it only accounts for
# just 5.6% of the variability, which is miniscule.

#5 things to interpret:
# relation between predictor and dependent variable (+ of - slope)
# the slope 
# the intercept
# significance --> p-value
# R^2 value

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.

model2 <- aov(kid_score ~ mom_hs + mom_iq, data = data)
summary(model2)

model2_2 <-lm(formula = kid_score ~ mom_hs + mom_iq, data = data)
summary(model2_2)

# considering the 2 predictors, the regression model P-values for mom_hs, and mom_iq are all
# less than the alpha level of 0.05. This suggests that the variability in kid_score are influenced by both mom_hs
# and mom_iq.

# Correlation: there are positive correlations between mom_hs and kid_score as well as mom_iq and kid_score.
# For every unit point increase in mom's iq score, kid_score increases by 0.56391 unit points.
# Mom's hs degree increases kid's iq_score by 5.95012 unit points.
# The intercept is 25.73, but in this case it's not really sensible as it's not realistic to have an iq of 0.
# The R-squared of 0.2141, considering that the R-squared in the single variable model is 0.561, suggests that
# delta R-squared (0.2141 - 0.561) is ~0.15. This means that 15% of the variability in kid_score can be attributed
# to mom_oq variable.

# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without degree in another color. Then also fit two separate regression lines 
#    such that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, 
#    kid_score_pred=fitted(your_model))

pred = data.frame(mom_iq=data$mom_iq, mom_hs=data$mom_hs, kid_score_pred = fitted(model2))
data$mom_hs <- as.factor(data$mom_hs)
ggplot(data, aes(mom_iq, kid_score, color = mom_hs)) +
  geom_point() +
  geom_line(aes(y = pred$kid_score_pred)) +
  labs(color = 'mom_hs') +
  ggtitle('Kid_score and mom_iq/mon_hs plot')

# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Fit the model and interpret your results.

model3 <- aov(kid_score ~ mom_hs*mom_iq, data) 
summary(model3)

model3_2 <-lm(formula = kid_score ~ mom_hs * mom_iq, data = data)
summary(model3_2)


# Interpretation
# the -0.4843 in the mom_hs:mom_iq affects the slop of mom_iq on kid_score. This means that the interaction
# between mom_hs:mom_iq decreases the slope of kid_score ~ mom_iq regression line by 0.4843.
# The model's R^2 value of 0.2301 is higher than the previous model

# Additionally, mom_hs:mom_iq is less than alpha level 0.05. This suggests, 
# that there's also a relationship between mom_hs and mom_iq.

# g) Next, let's plot the results of this model.
ggplot(data = data, aes(x = mom_iq, y = kid_score, col = mom_hs)) +
  geom_point()+
  geom_smooth(method = 'lm', aes(group = data$mom_hs, col = data$mom_hs))

plot(model2, data, which = seq(1:6))


# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the corresponding
#    child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.

predict(model3, data.frame(mom_hs = 1, mom_iq = 100), level = 0.95)


# i) Meaning of confidence intervals for regression line.
#    Let's go back to exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of this confidence interval?

# Close to mom_iq of 100, the confidence interval is narrower while it gets broader closer to the edges of
# the spectrum. This confidence interval shows the probability that the real model  
# will lie within the confidence interval of the regression model fitted that we have calculated.

# j) Finally, do model checking on your model from f), i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.
par(mfcol = c(2,3))
plot(lm(kid_score ~ mom_iq + mom_hs, data), which = seq(1:6))

# The plots of model3 and the standard model don't show any major differences (while model and model2 do).
# This suggests, that the third model is the best model for the given data.
# The plot suggests that our assumptions for linear regression hold. 
# The residuals vs fitted plot confirms the homogeneity of variance as the plotted points are uniformly scattered.
# The Normal Q-Q plot that appears to be a straight line suggests that the residuals follow a normal distriubition.
# The Cook's Distance plot shows that data observations 7, 213, and 286 may significantly affect the model
# prediction if pruned.
# The residuals vs leverage plot similarly shows this as these observation points' residuals are also larger than
# the other observation data points.
# The Cook's distance vs Leverage plot shows that most of the observation data points do not have high leverage,
# only the previously mentioned observations. 
# Based on these plots checking that the key assumptions for linear regression hold for our data, we should
# be able to use our fitted model to make some conclusions about the data.

