### Stats with R Exercise Sheet 3

##########################
#Week4: Hypothesis Testing
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 18th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 


## Please write below your (and all of your teammates') name, matriculation number. 
## Name: Noon Pokaratsiri Goldstein, Pauline Sander, Axel Allen
## Matriculation number: 2581469, 2581310, 2579024

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)


###############
### Exercise 1: Deriving sampling distributions
###############
## In this exercise, we're going to derive sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.
library(languageR)
help(dative)
summary(dative)
## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?
table(dative$LengthOfTheme)
# This table shows how many themes of a certain length can be found in the corpus.

## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?
hist(dative$LengthOfTheme)
boxplot(dative$LengthOfTheme)
# There are many outliers in the data as there are multiple data points outside(above) the whisker
# in the boxplot.
# From the histogram plot, the data is also skewed to the right (tail is on the right side). 

## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?
# Distribution in general describes the frequency of each possible outcome occurence in a number of trials.
# A sampling distribution is a probability distribution obtained obtained from sampling (randomly) drawn from
# a specific population.

## e) We are going to need a random sample of the variable 'LengthOfTime'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'
# There's no variable called 'LengthOfTime'; will assume that you meant 'LengthOfTheme' as that's in the data set
randomsampleoflengths <- sample(dative$LengthOfTheme, 5)

## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 
randomsampleoflengths2 <- sample(dative$LengthOfTheme, 5)

## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.
means5 <- c(mean(randomsampleoflengths), mean(randomsampleoflengths2))

## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.
means5 <- mean(sample(dative$LengthOfTheme, 5))
for (i in 1:999){
  means5 <- append(means5, mean(sample(dative$LengthOfTheme, 5)))
}

## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.
means50 <- mean(sample(dative$LengthOfTheme, 50))
for (i in 1:999){
  means50 <- append(means50, mean(sample(dative$LengthOfTheme, 50)))
}

## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?
# In means5, for each iteration of the 1000 samplings, the mean is calculated from the 5 randomly sampled numbers.
# In means50, for each iteration of the 1000 samplings, the mean is calculated from 50 sampled numbers instead.

## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 have a positive or negative skew?
hist(means5, breaks = 15)
hist(means50, breaks = 15)
# means5 have a positive skew (i.e. right skewed)

## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?
# As sampling size increases, the sample distribution tends towards the normal distribution according to the central
# limiti theorem. Thus, the more extreme values (min or max) fall within a certain distance from the normal 
# distribution mean. When sample size is smaller (e.g. 5), the distribution doesn't follow the normal distribution
# and so the extreme values can be much higher than if the sample were to follow normal distribution.

###############
### Exercise 2: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?


## b) Let's calculate the confidence interval for our means from the previous 
##    exercise.
##    First, install and load the packages 'lsr' and 'sciplot'


## c) Look at the description of the function ciMean to see which arguments it takes.


## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the mean for the variable LengthOfTheme.


## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?


## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.


## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?



###############
### Exercise 3: Plotting graphs using ggplot.
###############
# There are many ways of making graphs in R, and each has their own advantages 
# and disadvantages. One popular package for making plots is ggplot2. 
# The graphs produced with ggplot2 look professional and the code is quite easy 
# to manipulate.
# In this exercise, we'll plot a few graphs with ggplot2 to show its functionalities.
# You'll find all the information you'll need about plotting with ggplot2 here: 
# http://www.cookbook-r.com/Graphs/
# Also, you have been assigned the ggplot2 course in DataCamp. Please work through 
# this course.

## a) First install and load the ggplot2 package. Look at the help for ggplot.
install.packages("ggplot2")
library(ggplot2)

## b) We're going to be plotting data from the dataframe 'ratings' 
##    (included in languageR). 
##    Look at the description of the dataset and the summary.
help(ratings)
summary(ratings)

## For each word, we have three ratings (averaged over subjects), one for the 
## weight of the word's referent, one for its size, and one for the words' 
## subjective familiarity. Class is a factor specifying whether the word's 
## referent is an animal or a plant. 
## Furthermore, we have variables specifying various linguistic properties, 
## such as word's frequency, its length in letters, the number of synsets 
## (synonym sets) in which it is listed in WordNet [Miller, 1990], its 
## morphological family size (the number of complex words in which 
## the word occurs as a constituent), and its derivational entropy (an 
## information theoretic variant of the family size measure). 
## Don't worry, you don't have to know what all this means yet in order to 
## be able to plot it in this exercise!

## c) Let's look at the relationship between the class of words and the length. 
##    In order to plot this, we need a dataframe with the means.
##    Below you'll find the code to create a new dataframe based on the existing 
##    dataset ratings.
##    Plot a barplot of ratings.2 using ggplot. Map the two classes to two 
##    different colours. 
##    Remove the legend.
summary(ratings)
condition <- c("animal", "plant")
frequency <- c(mean(subset(ratings, Class == "animal")$Frequency), mean(subset(ratings, Class == "plant")$Frequency))
length <- c(mean(subset(ratings, Class == "animal")$Length), mean(subset(ratings, Class == "plant")$Length))
ratings.2 <- data.frame(condition, frequency, length)
ratings.2
ggplot(ratings.2, aes(x = length, fill = condition)) + 
  geom_histogram(aes(y = ..density..), show.legend = FALSE)

ggplot(ratings.2, aes(x = length, fill = condition)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE)


## d) Let's assume that we have additional data on the ratings of words. 
##    This data divides the conditions up into exotic and common animals 
##    and plants.
##    Below you'll find the code to update the dataframe with this additional data.
##    Draw a line graph with multiple lines to show the relationship between 
##    the frequency of the animals and plants and their occurrence.
##    Map occurrence to different point shapes and increase the size 
##    of these point shapes.
condition <- c("animal", "plant")
frequency <- c(7.4328978, 3.5864538)
length <- c(5.15678625, 7.81536584)
ratings.add <- data.frame(condition, frequency, length)
ratings.3 <- rbind(ratings.2, ratings.add)
occurrence <- c("common", "common", "exotic", "exotic")
ratings.3 <- cbind(ratings.3, occurrence)
ratings.3

# I'm confused here by the instruction whether or not to produce a line graph or a scattered plot
# the mapping instruction seems to specify point shapes and size, which would be more compatible with
# a scattered plot than a line graph. Also, there's only 1 data point per each category so I'm not sure
# how R can draw a line with just 1 point per class????? 
ggplot(ratings.3, aes(x = length, y = frequency, shape = occurrence, col = condition, size = frequency)) + 
  geom_point()
  

## e) Based on the graph you produced in question d, 
##    what can you conclude about how frequently 
##    people talk about plants versus animals, 
##    with regards to how common they are?
# In general, people talk more frequently about animals than they do plants.
# With respect to animals, people talk more frequently about exotic animals than they do common ones.
# On the other hand, people talke more frequently about common plants than they do exotic ones.