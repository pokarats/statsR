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

theme_update(plot.title = element_text(hjust = 0.5))
ggplot(data = data, aes(mom_iq, kid_score)) +
  ggtitle('Plot of kid_score and mom_iq') +
  geom_point(shape = 21) +
  geom_smooth(method = 'lm', se = F)
  



# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.
model <- lm(kid_score ~ mom_hs, data = data)
summary(model)


# based on the F-statistic p-value of 5.957 * 10^-7 at alpha = 0.05, the variability in kid_score can be said
# to be influenced by mom_hs. The slope of 11.771 on the regression line suggests that for every unit point
# increase in mom_hs, kid_score also increases by the factor of 11.771.

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.
summary(aov(kid_score ~ mom_hs * mom_iq, data = data))

# considering the 2 predictors, the regression model P-values for mom_hs, mom_iq, and mom_hs:mom_iq are all
# less than the alpha level of 0.05. This suggests that the variability in kid_score are influenced by both mom_hs
# and mom_iq and that there's also a relationship between mom_hs and mom_iq.

# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without degree in another color. Then also fit two separate regression lines 
#    such that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, 
#    kid_score_pred=fitted(your_model))



# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Fit the model and interpret your results.



# g) Next, let's plot the results of this model.



# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the corresponding
#    child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.



# i) Meaning of confidence intervals for regression line.
#    Let's go back to exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of this confidence interval?



# j) Finally, do model checking on your model from f), i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.


