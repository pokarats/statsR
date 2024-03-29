### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorical Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 25th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Noon Pokaratsiri Goldstein, Pauline Sander, Axel Allen
## Matriculation number: 2581469, 2581310, 2579024

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.
#P(success for each question) = 1/5, P(failure for each question) = 4/5
p = 1/5
q = 4/5
num_trials = 12
dbinom(4,num_trials,p)
# getting exactly a certain number of outcome can only be evaluated on discrete variables

## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 
# (P(X <= 4))
#sum(dbinom for x = 0 to 4) or use pbinom for cumulative probability
#take the lower tail so leave the default
pbinom(4,num_trials,p)
# or
sum(dbinom(0:4,num_trials,p))

##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
library(languageR)
summary(dutchSpeakersDistMeta)
str(dutchSpeakersDistMeta)
# Speaker, Sex, AgeGroup, ConversationType, EduLevel are factors.

## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.
(dutchtable <- table(dutchSpeakersDistMeta$AgeGroup, dutchSpeakersDistMeta$Sex))

##    Visualize your data with a single bar plot (use ggplot) that represents the counts with 
##    respect to each age group and each sex.
library(ggplot2)
ggplot(dutchSpeakersDistMeta, aes(x=AgeGroup, fill=Sex))+
  geom_bar(position = "dodge")

## c) Inspect the table you created in b). Does it look like there could be a significant 
##    difference between the sexes?
# For both sexes, there are more speakers in the younger age groups. However, there are more female
# speakers than male speakers in all age groups exept one. 
# Whether that difference is significant, it is difficult to say with any degrees of certainty
# without any calculations.

## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group using the function chisq.test. 
##    Look at the help of this function. 
##    Then use the  function to calculate whether there's a difference in our table from b). 
##    Is there a significant difference in age group?
?chisq.test
chisq.test(dutchtable)
# The p value  (0.51) doesn't suggest a significant difference.
# the p value (that corresponds to the chi-square value) tells us the probability of observing our 
# results or data GIVEN that the NULL hypothesis is true. Hence, you want a smaller P-value so that you
# can reject the NULL hypothesis and attribute your observed differences to something (NOT by chance.)

## e) What are the degrees of freedom for our data? How are they derived?
# (C-1)(R-1) = (5-1)(2-1) = 4
#  (number of categories i.e. columns - 1)(number of subcategories i.e. rows - 1)

##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll consider a paper on therapeutic touch 
##    (google it, if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    Several practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?
# Null hypothesis: There is no such thing as therapeutic touch. If the participants
# guessed right, this happened by pure chance.
# In this case they should be right about 50% of the time (140 out of 280).

## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 

## here are the correctiosn from the tutorial
type <- c('correct', 'incorrect')
expected <- c(140, 140)
observed <- c(123, 157)
total <- c(280, 280)
chisq.test(observed)
# X-squareed = 4.1286, df = 1, p-value = 0.04216
# so, we can reject the NULL hypothesis

# ignore these below: incorrect answer
# (123-140)^2/140 = -17^2/140 = 289/140 = 2.06
pchisq(2.06, 1)
# = 0.8487898
# The null-hypothesis should not be rejected.

## c) Now calculate significance using the binomial test as we used it in exercise 1.
pbinom(123, size=280, 0.5, lower.tail = T)
# = 0.02420056

# for 2 tailed test, need to multiple area of the pbinom by 2; remember this distribution is approximated
# by the normal distribution
pbinom(123, size=280, 0.5, lower.tail = T) * 2
# p = 0.04840113, which is comparable to the p-value of 0.04216 from the X-squared distribution; 
# X-squared distribution only has 1 tail

# --- ignore below: incorrect answers
# cf. slide 9-10 lec. 5
# Need to calculate probability that they manage to get 123 or more trials
# right by chance.The null-hypothesis should not be rejected.

## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?
# the binomial is more representative of the distribution of the sample; so that is more exact and accurate in
# this case since we're comparing 2 outcomes. The X-squared is an approximation.

# In this case (when there are only 2 categories), the binomial test should be used, simply because it is possible.
# ChiSquare is only a proxy for those cases, for which it is not possible to use the
# binomial test.

##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

# A McNemars test should be used if there are dependencies between the
# observations. This can be the case, if two measurements have been taken for 
# each individuum, one before and one after a treatment. Then, the measurements
# should be put together in pairs and counted as those, using a McNemar's test.
# Counting the measurements separately and using the normal ChiSquare test would
# 1) result in a completely different contingency table.
# 2) not account for the change between two data points.
# 3) include too many data points.
# It would thus not give the desired outcome.