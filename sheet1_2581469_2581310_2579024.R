### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 4. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number. 
## Name:Noon Pokaratsiri Goldstein, Pauline Sander, Axel Allen
## Matriculation number:2581469, 2581310, 2579024

## Change the name of the file by adding your matriculation numbers
## (sheet01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()

## b) Get help with this function.
?getwd() #or 
help(getwd())


## c) Change your working directory to another directory.
#setwd(/path/to/another/working/directory) #I commented this out because I don't actually want to change my wd



###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?
library(languageR)
head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)
#head shows the first numbers of rows of data, in this case, Speakers N01001-N01006.
#tail shows the last numbers of rows of data, in this case, Speakers N01216-N01221.
#summary shows a run down of the data and basic statistics such as the quartiles, min, max, mean,
#and counts by variable categories

## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.
nrow(dutchSpeakersDistMeta)

## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.
boxplot(AgeYear ~ Sex, data = dutchSpeakersDistMeta)



## e) Does it seem as if either of the two groups has more variability in age?
#yes, the female group has wider variability than the male group. 

## f) Do you see any outliers in either of the two groups?
#yes, The male group also has a few extreme data points outside of the whiskers in the boxplots

## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?
library(dplyr)
dutchSpeakersDistMeta %>% 
  group_by(Sex) %>%
  summarise(Mean = mean(AgeYear, na.rm = TRUE))

dutchSpeakersDistMeta %>% 
  group_by(Sex) %>%
  summarise(StandardDev = sd(AgeYear, na.rm = TRUE))
#The two groups seem to have almost the same means and standard deviations, suggesting that they do not differ
#much in age.

## h) What do the whiskers of a boxplot mean?
#The whiskers go from the Q1 and Q3 points of the data to the min and max respectively.

###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)
# The data's measurement scale is discrete because 'and then' cannot be used 
# 14.5 times. It is an interval scale because the data can be ordered
# and the steps between 15 and 16 and 16 and 17 are equidistant.

## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?
# In a dataframe the row names have to be unique. This is what we want for the IDs.
# Furthermore, dataframes allow columns to be of different types.

## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25
pps <- c(1:25)

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)

## e) Create a dataframe for this data. Assign this to 'stories'. 
stories <- data.frame(obs, row.names = pps)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?
class(pps)
# pps is of class "integer".

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?
pps <- factor(pps)
# The IDs should be categorial (e.g. we don't want do calculate a mean). By 
# changing the class to factor this requirement is fulfilled.

## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.
hist(stories$obs, breaks = 8)

## i) Create a kernel density plot using density().
d <- density(stories$obs)
plot(d)

## j) What is the difference between a histogram and a kernel density plot?
# The kernel density plot doesn't show the abso frequencies in the data set
# as does the histogram (which additionally splits the set into more or less
# arbitrary subsets). Instead, it shows how dense the data points are around
# a given value.

## This is a difficult one, remember you just need to provide a serious attempt at solving each 
## exercise in order to pass. 
## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)
hist(stories$obs, breaks = 8, prob = TRUE)
lines(d)


###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.
x = seq(-5,5, by = .1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
y = dnorm(x)

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x,y)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.
plot(x,y,type="line", ylim = c(0.0,0.8))

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
?abline
abline(v = mean(x), lty=2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".
beaver1
b1temp <- beaver1$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
b1temp_mean = mean(b1temp)
b1temp_std = sd(b1temp)
y = dnorm(b1temp, mean = b1temp_mean, sd = b1temp_std)
plot(b1temp,y)

## h) We observe two temparatures (36.91 and 38.13). What's the likelihood that
##    these temperatures (or more extreme ones) respectively come 
##    from the normal distribution from g)?
#Is the question asking, what's the probability that a randomly sampled temperature be between 36.91 and 38.13?
#i.e. what's P(36.91 <= X <= 38.13 : X ~ N(b1temp_mean, b1temp_std))?
upperbound = pnorm(38.13, b1temp_mean, b1temp_std) #P(X <= 38.13; i.e. integral of PDF from x=0 -> x=38.13)
lowerbound = 1 - pnorm(36.91, b1temp_mean, b1temp_std) # 1 - P(X < 36.13); 1 - integral of PDF from x=0 -> x=36.91
upperbound - lowerbound #probabily that a randomly sampled temperature will be between [36.91, 38.13]

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. What do you observe?
for (i in c(1:5)) {
  random_samples = rnorm(20, b1temp_mean, b1temp_std)
  hist(random_samples, plot = TRUE)
}
#Observations: Only some of the histogram plots look like a normal distribution. Some are multi-modal or
#have a number of extrem values